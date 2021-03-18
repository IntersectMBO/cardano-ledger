{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Rules.Bbody
  ( AlonzoBBODY,
    bbodyTransition,
  )
where

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (Tx)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (BlockDecoding, Era (Crypto))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
    (?!),
  )
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, UnitInterval, epochInfo)
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (bhash, bheaderSlotNo),
    BHeader (..),
    Block (..),
    bBodySize,
    bbHash,
    hBbsize,
    incrBlocks,
    issuerIDfromBHBody,
    txSeqTxns,
  )
import Shelley.Spec.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Shelley.Spec.Ledger.LedgerState (LedgerState)
import Shelley.Spec.Ledger.OverlaySchedule (isOverlaySlot)
import Shelley.Spec.Ledger.STS.Bbody
  ( BbodyEnv (..),
    BbodyPredicateFailure (..),
    BbodyState (..),
  )
import Shelley.Spec.Ledger.STS.Ledgers (LedgersEnv (..))
import Shelley.Spec.Ledger.Slot (epochInfoEpoch, epochInfoFirst)
import Shelley.Spec.Ledger.TxBody (EraIndependentTxBody)

-- =======================================
-- A new PredicateFailure type

data AlonzoBbodyPredFail era
  = ShelleyInAlonzoPredFail (BbodyPredicateFailure era)
  | TooManyExUnits
      !ExUnits
      -- ^ Computed Sum of ExUnits for all plutus scripts
      !ExUnits
      -- ^ Maximum allowed by protocal parameters
  deriving (Generic)

deriving instance
  (Era era, Show (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  Show (AlonzoBbodyPredFail era)

deriving instance
  (Era era, Eq (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  Eq (AlonzoBbodyPredFail era)

deriving anyclass instance
  (Era era, NoThunks (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  NoThunks (AlonzoBbodyPredFail era)

-- TODO  Do we need CBOR instances? he shelley (BbodyPredicateFailure era) doesn't seem to have one.

-- ========================================
-- The STS instance

-- | The uninhabited type that marks the STS Alonzo Era instance.
data AlonzoBBODY era

bbodyTransition ::
  forall (someBBODY :: Type -> Type) era.
  ( -- Conditions that the Abstract someBBODY must meet
    STS (someBBODY era),
    Signal (someBBODY era) ~ Block era,
    PredicateFailure (someBBODY era) ~ AlonzoBbodyPredFail era,
    BaseM (someBBODY era) ~ ShelleyBase,
    State (someBBODY era) ~ BbodyState era,
    Environment (someBBODY era) ~ BbodyEnv era,
    -- Conditions to be an instance of STS
    Embed (Core.EraRule "LEDGERS" era) (someBBODY era),
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Core.Tx era),
    -- Conditions to define the rule in this Era
    BlockDecoding era,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_maxBlockExUnits" (Core.PParams era) ExUnits,
    HasField "totExunits" (Core.Tx era) ExUnits,
    Era era -- supplies WellFormed HasField, and Crypto constraints
  ) =>
  TransitionRule (someBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp account,
               BbodyState ls b,
               Block (BHeader bhb _) txsSeq
               )
           ) -> do
        let txs = txSeqTxns txsSeq
            actualBodySize = bBodySize txsSeq
            actualBodyHash = bbHash txsSeq

        actualBodySize == fromIntegral (hBbsize bhb)
          ?! (ShelleyInAlonzoPredFail $ WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ hBbsize bhb))

        actualBodyHash == bhash bhb
          ?! (ShelleyInAlonzoPredFail $ InvalidBodyHashBBODY @era actualBodyHash (bhash bhb))

        ls' <-
          trans @(Core.EraRule "LEDGERS" era) $
            TRC (LedgersEnv (bheaderSlotNo bhb) pp account, ls, StrictSeq.fromStrict txs)

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole . issuerIDfromBHBody $ bhb
            slot = bheaderSlotNo bhb
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfo
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e

        let txTotal, ppMax :: ExUnits
            txTotal = foldr (<>) mempty (fmap (getField @"totExunits") txs)
            ppMax = getField @"_maxBlockExUnits" pp
        txTotal <= ppMax ?! TooManyExUnits txTotal ppMax

        pure $
          BbodyState @era
            ls'
            ( incrBlocks
                (isOverlaySlot firstSlotNo (getField @"_d" pp) slot)
                hkAsStakePool
                b
            )

instance
  ( DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Embed (Core.EraRule "LEDGERS" era) (AlonzoBBODY era),
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Alonzo.Tx era),
    Era era,
    Core.Tx era ~ Alonzo.Tx era,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_maxBlockExUnits" (Core.PParams era) ExUnits,
    BlockDecoding era
  ) =>
  STS (AlonzoBBODY era)
  where
  type
    State (AlonzoBBODY era) =
      BbodyState era

  type
    Signal (AlonzoBBODY era) =
      Block era

  type Environment (AlonzoBBODY era) = BbodyEnv era

  type BaseM (AlonzoBBODY era) = ShelleyBase

  type PredicateFailure (AlonzoBBODY era) = AlonzoBbodyPredFail era

  initialRules = []
  transitionRules = [bbodyTransition @AlonzoBBODY]

instance
  ( Era era,
    BaseM ledgers ~ ShelleyBase,
    ledgers ~ Core.EraRule "LEDGERS" era,
    STS ledgers,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Era era
  ) =>
  Embed ledgers (AlonzoBBODY era)
  where
  wrapFailed = ShelleyInAlonzoPredFail . LedgersFailure
