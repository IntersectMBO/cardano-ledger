{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Bbody (
  AlonzoBBODY,
  AlonzoBbodyPredFailure (..),
  AlonzoBbodyEvent (..),
  bbodyTransition,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Era (AlonzoBBODY, AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppMaxBlockExUnitsL)
import Cardano.Ledger.Alonzo.Rules.Ledgers ()
import Cardano.Ledger.Alonzo.Rules.Utxo (AlonzoUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), pointWiseExUnits)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx, totExUnits)
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq, txSeqTxns)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.BHeaderView (BHeaderView (..), isOverlaySlot)
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfoPure)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Cardano.Ledger.Shelley.BlockChain (incrBlocks)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBbodyEvent (..),
  ShelleyBbodyPredFailure (..),
  ShelleyBbodyState (..),
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure,
  ShelleyDelplPredFailure,
  ShelleyLedgerPredFailure,
  ShelleyLedgersEnv (..),
  ShelleyLedgersPredFailure,
  ShelleyPoolPredFailure,
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import Cardano.Ledger.Slot (epochInfoEpoch, epochInfoFirst)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Embed (..),
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
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

-- =======================================
-- A new PredicateFailure type

data AlonzoBbodyPredFailure era
  = ShelleyInAlonzoBbodyPredFailure (ShelleyBbodyPredFailure era)
  | TooManyExUnits
      -- | Computed Sum of ExUnits for all plutus scripts
      !ExUnits
      -- | Maximum allowed by protocal parameters
      !ExUnits
  deriving (Generic)

newtype AlonzoBbodyEvent era
  = ShelleyInAlonzoEvent (ShelleyBbodyEvent era)

type instance EraRuleFailure "BBODY" (AlonzoEra c) = AlonzoBbodyPredFailure (AlonzoEra c)

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure (AlonzoEra c)

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure

instance InjectRuleFailure "BBODY" ShelleyLedgerPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPpupPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegsPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelplPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

deriving instance
  (Era era, Show (PredicateFailure (EraRule "LEDGERS" era))) =>
  Show (AlonzoBbodyPredFailure era)

deriving instance
  (Era era, Eq (PredicateFailure (EraRule "LEDGERS" era))) =>
  Eq (AlonzoBbodyPredFailure era)

deriving anyclass instance
  (Era era, NoThunks (PredicateFailure (EraRule "LEDGERS" era))) =>
  NoThunks (AlonzoBbodyPredFailure era)

instance
  ( Typeable era
  , EncCBOR (ShelleyBbodyPredFailure era)
  ) =>
  EncCBOR (AlonzoBbodyPredFailure era)
  where
  encCBOR (ShelleyInAlonzoBbodyPredFailure x) = encode (Sum ShelleyInAlonzoBbodyPredFailure 0 !> To x)
  encCBOR (TooManyExUnits x y) = encode (Sum TooManyExUnits 1 !> To x !> To y)

instance
  ( Typeable era
  , DecCBOR (ShelleyBbodyPredFailure era) -- TODO why is there no DecCBOR for (ShelleyBbodyPredFailure era)
  ) =>
  DecCBOR (AlonzoBbodyPredFailure era)
  where
  decCBOR = decode (Summands "AlonzoBbodyPredFail" dec)
    where
      dec 0 = SumD ShelleyInAlonzoBbodyPredFailure <! From
      dec 1 = SumD TooManyExUnits <! From <! From
      dec n = Invalid n

-- ========================================
-- The STS instance

bbodyTransition ::
  forall (someBBODY :: Type -> Type) era.
  ( STS (someBBODY era)
  , Signal (someBBODY era) ~ Block (BHeaderView (EraCrypto era)) era
  , PredicateFailure (someBBODY era) ~ AlonzoBbodyPredFailure era
  , BaseM (someBBODY era) ~ ShelleyBase
  , State (someBBODY era) ~ ShelleyBbodyState era
  , Environment (someBBODY era) ~ BbodyEnv era
  , Embed (EraRule "LEDGERS" era) (someBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , EraSegWits era
  , AlonzoEraTxWits era
  , Era.TxZones era ~ AlonzoTxSeq era
  , Tx era ~ AlonzoTx era
  , AlonzoEraPParams era
  ) =>
  TransitionRule (someBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
            ( BbodyEnv pp account
              , BbodyState ls b
              , UnserialisedBlock bh txsSeq
              )
          ) -> do
        let txs = txSeqTxns txsSeq
            actualBodySize = bBodySize (pp ^. ppProtocolVersionL) txsSeq
            actualBodyHash = hashTxZones @era txsSeq

        actualBodySize
          == fromIntegral (bhviewBSize bh)
            ?! ShelleyInAlonzoBbodyPredFailure
              ( WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ bhviewBSize bh)
              )

        actualBodyHash
          == bhviewBHash bh
            ?! ShelleyInAlonzoBbodyPredFailure
              ( InvalidBodyHashBBODY @era actualBodyHash (bhviewBHash bh)
              )

        ls' <-
          trans @(EraRule "LEDGERS" era) $
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
          ei <- asks epochInfoPure
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e

        {- ∑(tx ∈ txs)(totExunits tx) ≤ maxBlockExUnits pp  -}
        let txTotal, ppMax :: ExUnits
            txTotal = foldMap totExUnits txs
            ppMax = pp ^. ppMaxBlockExUnitsL
        pointWiseExUnits (<=) txTotal ppMax ?! TooManyExUnits txTotal ppMax

        pure $
          BbodyState @era
            ls'
            ( incrBlocks
                (isOverlaySlot firstSlotNo (pp ^. ppDG) slot)
                hkAsStakePool
                b
            )

instance
  ( DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Embed (EraRule "LEDGERS" era) (AlonzoBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (AlonzoTx era)
  , AlonzoEraTxWits era
  , Tx era ~ AlonzoTx era
  , Era.TxZones era ~ AlonzoTxSeq era
  , Tx era ~ AlonzoTx era
  , EraSegWits era
  , AlonzoEraPParams era
  ) =>
  STS (AlonzoBBODY era)
  where
  type
    State (AlonzoBBODY era) =
      ShelleyBbodyState era

  type
    Signal (AlonzoBBODY era) =
      (Block (BHeaderView (EraCrypto era)) era)

  type Environment (AlonzoBBODY era) = BbodyEnv era

  type BaseM (AlonzoBBODY era) = ShelleyBase

  type PredicateFailure (AlonzoBBODY era) = AlonzoBbodyPredFailure era
  type Event (AlonzoBBODY era) = AlonzoBbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition @AlonzoBBODY]

instance
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Era era
  ) =>
  Embed ledgers (AlonzoBBODY era)
  where
  wrapFailed = ShelleyInAlonzoBbodyPredFailure . LedgersFailure
  wrapEvent = ShelleyInAlonzoEvent . LedgersEvent
