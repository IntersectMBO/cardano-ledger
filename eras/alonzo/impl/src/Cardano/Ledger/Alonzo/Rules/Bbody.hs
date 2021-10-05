{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
    AlonzoBbodyPredFail (..),
    AlonzoBbodyEvent (..),
    bbodyTransition,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), pointWiseExUnits)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (ValidatedTx, totExUnits)
import Cardano.Ledger.Alonzo.TxSeq (txSeqTxns)
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo (TxSeq)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness)
import Cardano.Ledger.BHeaderView (BHeaderView (..), isOverlaySlot)
import Cardano.Ledger.BaseTypes (ShelleyBase, UnitInterval, epochInfo)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto), SupportsSegWit (..))
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Cardano.Ledger.Shelley.BlockChain (bBodySize, incrBlocks)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Cardano.Ledger.Shelley.Rules.Bbody
  ( BbodyEnv (..),
    BbodyEvent (..),
    BbodyPredicateFailure (..),
    BbodyState (..),
  )
import Cardano.Ledger.Shelley.Rules.Ledgers (LedgersEnv (..))
import Cardano.Ledger.Shelley.TxBody (EraIndependentTxBody)
import Cardano.Ledger.Slot (epochInfoEpoch, epochInfoFirst)
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
import Data.Coders
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))

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

newtype AlonzoBbodyEvent era
  = ShelleyInAlonzoEvent (BbodyEvent era)

deriving instance
  (Era era, Show (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  Show (AlonzoBbodyPredFail era)

deriving instance
  (Era era, Eq (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  Eq (AlonzoBbodyPredFail era)

deriving anyclass instance
  (Era era, NoThunks (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  NoThunks (AlonzoBbodyPredFail era)

instance
  ( Typeable era,
    ToCBOR (BbodyPredicateFailure era)
  ) =>
  ToCBOR (AlonzoBbodyPredFail era)
  where
  toCBOR (ShelleyInAlonzoPredFail x) = encode (Sum ShelleyInAlonzoPredFail 0 !> To x)
  toCBOR (TooManyExUnits x y) = encode (Sum TooManyExUnits 1 !> To x !> To y)

instance
  ( Typeable era,
    FromCBOR (BbodyPredicateFailure era) -- TODO why is there no FromCBOR for (BbodyPredicateFailure era)
  ) =>
  FromCBOR (AlonzoBbodyPredFail era)
  where
  fromCBOR = decode (Summands "AlonzoBbodyPredFail" dec)
    where
      dec 0 = SumD ShelleyInAlonzoPredFail <! From
      dec 1 = SumD TooManyExUnits <! From <! From
      dec n = Invalid n

-- ========================================
-- The STS instance

-- | The uninhabited type that marks the STS Alonzo Era instance.
data AlonzoBBODY era

bbodyTransition ::
  forall (someBBODY :: Type -> Type) era.
  ( -- Conditions that the Abstract someBBODY must meet
    STS (someBBODY era),
    Signal (someBBODY era) ~ (BHeaderView (Crypto era), TxSeq era),
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
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_maxBlockExUnits" (Core.PParams era) ExUnits,
    Era era, -- supplies WellFormed HasField, and Crypto constraints
    Era.TxSeq era ~ Alonzo.TxSeq era,
    Core.Tx era ~ Alonzo.ValidatedTx era,
    Core.Witnesses era ~ TxWitness era
  ) =>
  TransitionRule (someBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp account,
               BbodyState ls b,
               (bh, txsSeq)
               )
           ) -> do
        let txs = txSeqTxns txsSeq
            actualBodySize = bBodySize txsSeq
            actualBodyHash = hashTxSeq @era txsSeq

        actualBodySize == fromIntegral (bhviewBSize bh)
          ?! ShelleyInAlonzoPredFail
            ( WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ bhviewBSize bh)
            )

        actualBodyHash == bhviewBHash bh
          ?! ShelleyInAlonzoPredFail
            ( InvalidBodyHashBBODY @era actualBodyHash (bhviewBHash bh)
            )

        ls' <-
          trans @(Core.EraRule "LEDGERS" era) $
            TRC (LedgersEnv (bhviewSlot bh) pp account, ls, StrictSeq.fromStrict txs)

        -- Note that this may not actually be a stake pool - it could be a
        -- genesis key delegate. However, this would only entail an overhead of
        -- 7 counts, and it's easier than differentiating here.
        --
        -- TODO move this computation inside 'incrBlocks' where it belongs. Here
        -- we make an assumption that 'incrBlocks' must enforce, better for it
        -- to be done in 'incrBlocks' where we can see that the assumption is
        -- enforced.
        let hkAsStakePool = coerceKeyRole . bhviewID $ bh
            slot = bhviewSlot bh
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfo
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e

        {- ∑(tx ∈ txs)(totExunits tx) ≤ maxBlockExUnits pp  -}
        let txTotal, ppMax :: ExUnits
            txTotal = foldMap Alonzo.totExUnits txs
            ppMax = getField @"_maxBlockExUnits" pp
        pointWiseExUnits (<=) txTotal ppMax ?! TooManyExUnits txTotal ppMax

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
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Alonzo.ValidatedTx era),
    Era era,
    Core.Tx era ~ Alonzo.ValidatedTx era,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_maxBlockExUnits" (Core.PParams era) ExUnits,
    Era.TxSeq era ~ Alonzo.TxSeq era,
    Core.Tx era ~ Alonzo.ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    SupportsSegWit era
  ) =>
  STS (AlonzoBBODY era)
  where
  type
    State (AlonzoBBODY era) =
      BbodyState era

  type
    Signal (AlonzoBBODY era) =
      (BHeaderView (Crypto era), TxSeq era)

  type Environment (AlonzoBBODY era) = BbodyEnv era

  type BaseM (AlonzoBBODY era) = ShelleyBase

  type PredicateFailure (AlonzoBBODY era) = AlonzoBbodyPredFail era
  type Event (AlonzoBBODY era) = AlonzoBbodyEvent era

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
  wrapEvent = ShelleyInAlonzoEvent . LedgersEvent
