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
  alonzoBbodyTransition,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.BlockBody (AlonzoBlockBody)
import Cardano.Ledger.Alonzo.Era (AlonzoBBODY, AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppMaxBlockExUnitsL)
import Cardano.Ledger.Alonzo.Rules.Ledgers ()
import Cardano.Ledger.Alonzo.Rules.Utxo (AlonzoUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), pointWiseExUnits)
import Cardano.Ledger.Alonzo.Tx (totExUnits)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.BHeaderView (BHeaderView (..), isOverlaySlot)
import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (..), ShelleyBase, epochInfoPure)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Shelley.BlockBody (incrBlocks)
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
  | TooManyExUnits (Mismatch 'RelLTEQ ExUnits)
  deriving (Generic)

newtype AlonzoBbodyEvent era
  = ShelleyInAlonzoEvent (ShelleyBbodyEvent era)
  deriving (Generic)

type instance EraRuleFailure "BBODY" AlonzoEra = AlonzoBbodyPredFailure AlonzoEra

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure AlonzoEra

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure

instance InjectRuleFailure "BBODY" ShelleyLedgerPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPpupPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegsPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelplPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure AlonzoEra where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegPredFailure AlonzoEra where
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
  encCBOR (TooManyExUnits m) = encode (Sum TooManyExUnits 1 !> To m)

instance
  ( Typeable era
  , DecCBOR (ShelleyBbodyPredFailure era) -- TODO why is there no DecCBOR for (ShelleyBbodyPredFailure era)
  ) =>
  DecCBOR (AlonzoBbodyPredFailure era)
  where
  decCBOR = decode (Summands "AlonzoBbodyPredFail" dec)
    where
      dec 0 = SumD ShelleyInAlonzoBbodyPredFailure <! From
      dec 1 = SumD TooManyExUnits <! From
      dec n = Invalid n

-- ========================================
-- The STS instance

alonzoBbodyTransition ::
  forall era.
  ( STS (EraRule "BBODY" era)
  , Signal (EraRule "BBODY" era) ~ Block BHeaderView era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , EraBlockBody era
  , AlonzoEraTxWits era
  , BlockBody era ~ AlonzoBlockBody era
  , AlonzoEraPParams era
  ) =>
  TransitionRule (EraRule "BBODY" era)
alonzoBbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp account
               , BbodyState ls b
               , Block bh txsSeq
               )
           ) -> do
        let txs = txsSeq ^. txSeqBlockBodyL
            actualBodySize = bBodySize (pp ^. ppProtocolVersionL) txsSeq
            actualBodyHash = hashBlockBody @era txsSeq

        actualBodySize
          == fromIntegral (bhviewBSize bh)
            ?! injectFailure
              ( ShelleyInAlonzoBbodyPredFailure
                  ( WrongBlockBodySizeBBODY $
                      Mismatch
                        { mismatchSupplied = actualBodySize
                        , mismatchExpected = fromIntegral $ bhviewBSize bh
                        }
                  )
              )

        actualBodyHash
          == bhviewBHash bh
            ?! injectFailure
              ( ShelleyInAlonzoBbodyPredFailure
                  ( InvalidBodyHashBBODY @era $
                      Mismatch
                        { mismatchSupplied = actualBodyHash
                        , mismatchExpected = bhviewBHash bh
                        }
                  )
              )

        -- Note that this may not actually be a stake pool - it could be a
        -- genesis key delegate. However, this would only entail an overhead of
        -- 7 counts, and it's easier than differentiating here.
        --
        -- TODO move this computation inside 'incrBlocks' where it belongs. Here
        -- we make an assumption that 'incrBlocks' must enforce, better for it
        -- to be done in 'incrBlocks' where we can see that the assumption is
        -- enforced.
        let hkAsStakePool = coerceKeyRole $ bhviewID bh
            slot = bhviewSlot bh
        (firstSlotNo, curEpochNo) <- liftSTS $ do
          ei <- asks epochInfoPure
          let curEpochNo = epochInfoEpoch ei slot
          pure (epochInfoFirst ei curEpochNo, curEpochNo)

        ls' <-
          trans @(EraRule "LEDGERS" era) $
            TRC (LedgersEnv (bhviewSlot bh) curEpochNo pp account, ls, StrictSeq.fromStrict txs)

        {- ∑(tx ∈ txs)(totExunits tx) ≤ maxBlockExUnits pp  -}
        let txTotal, ppMax :: ExUnits
            txTotal = foldMap totExUnits txs
            ppMax = pp ^. ppMaxBlockExUnitsL
        pointWiseExUnits (<=) txTotal ppMax
          ?! injectFailure (TooManyExUnits Mismatch {mismatchSupplied = txTotal, mismatchExpected = ppMax})

        pure $
          BbodyState @era
            ls'
            ( incrBlocks
                (isOverlaySlot firstSlotNo (pp ^. ppDG) slot)
                hkAsStakePool
                b
            )

instance
  ( EraRule "BBODY" era ~ AlonzoBBODY era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , Embed (EraRule "LEDGERS" era) (AlonzoBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , AlonzoEraTxWits era
  , BlockBody era ~ AlonzoBlockBody era
  , EraBlockBody era
  , AlonzoEraPParams era
  ) =>
  STS (AlonzoBBODY era)
  where
  type
    State (AlonzoBBODY era) =
      ShelleyBbodyState era

  type
    Signal (AlonzoBBODY era) =
      (Block BHeaderView era)

  type Environment (AlonzoBBODY era) = BbodyEnv era

  type BaseM (AlonzoBBODY era) = ShelleyBase

  type PredicateFailure (AlonzoBBODY era) = AlonzoBbodyPredFailure era
  type Event (AlonzoBBODY era) = AlonzoBbodyEvent era

  initialRules = []
  transitionRules = [alonzoBbodyTransition @era]

instance
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  , Era era
  ) =>
  Embed ledgers (AlonzoBBODY era)
  where
  wrapFailed = ShelleyInAlonzoBbodyPredFailure . LedgersFailure
  wrapEvent = ShelleyInAlonzoEvent . LedgersEvent
