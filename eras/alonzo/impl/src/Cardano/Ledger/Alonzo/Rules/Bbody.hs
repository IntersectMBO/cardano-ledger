{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  validateExUnits,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Era (AlonzoBBODY, AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Ledgers ()
import Cardano.Ledger.Alonzo.Rules.Utxo (AlonzoUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), pointWiseExUnits)
import Cardano.Ledger.Alonzo.Tx (totExUnits)
import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Block (Block (..), EraBlockHeader (..))
import Cardano.Ledger.Shelley.BlockBody (incrBlocks)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  BbodySignal (..),
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
  validateBodyHash,
  validateBodySize,
 )
import Cardano.Ledger.Slot (slotToEpochBoundary)
import Control.DeepSeq (NFData)
import Control.State.Transition (
  Embed (..),
  Rule,
  RuleType (..),
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
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data AlonzoBbodyPredFailure era
  = ShelleyInAlonzoBbodyPredFailure (ShelleyBbodyPredFailure era)
  | TooManyExUnits (Mismatch RelLTEQ ExUnits)
  deriving (Generic)

instance NFData (PredicateFailure (EraRule "LEDGERS" era)) => NFData (AlonzoBbodyPredFailure era)

newtype AlonzoBbodyEvent era
  = ShelleyInAlonzoEvent (ShelleyBbodyEvent era)
  deriving (Generic)

deriving instance
  Eq (Event (EraRule "LEDGERS" era)) =>
  Eq (AlonzoBbodyEvent era)

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
  (Era era, EncCBOR (PredicateFailure (EraRule "LEDGERS" era))) =>
  EncCBOR (AlonzoBbodyPredFailure era)
  where
  encCBOR (ShelleyInAlonzoBbodyPredFailure x) = encode (Sum ShelleyInAlonzoBbodyPredFailure 0 !> To x)
  encCBOR (TooManyExUnits m) = encode (Sum TooManyExUnits 1 !> To m)

instance
  (Era era, DecCBOR (PredicateFailure (EraRule "LEDGERS" era))) =>
  DecCBOR (AlonzoBbodyPredFailure era)
  where
  decCBOR = decode (Summands "AlonzoBbodyPredFail" dec)
    where
      dec 0 = SumD ShelleyInAlonzoBbodyPredFailure <! From
      dec 1 = SumD TooManyExUnits <! From
      dec n = Invalid n

-- | Validate that total execution units (all transactions) do not exceed block limit.
validateExUnits ::
  forall era.
  ( AlonzoEraTx era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  ) =>
  StrictSeq.StrictSeq (Tx TopTx era) ->
  -- | Max block exunits protocol parameter.
  ExUnits ->
  Rule (EraRule "BBODY" era) 'Transition ()
validateExUnits txs ppMax =
  let txTotal = foldMap totExUnits txs
   in pointWiseExUnits (<=) txTotal ppMax
        ?! injectFailure
          ( TooManyExUnits $
              Mismatch
                { mismatchSupplied = txTotal
                , mismatchExpected = ppMax
                }
          )

alonzoBbodyTransition ::
  forall era.
  ( STS (EraRule "BBODY" era)
  , Signal (EraRule "BBODY" era) ~ BbodySignal era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" ShelleyBbodyPredFailure era
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , EraBlockBody era
  , AlonzoEraTx era
  ) =>
  TransitionRule (EraRule "BBODY" era)
alonzoBbodyTransition = do
  TRC (BbodyEnv pp account, BbodyState ls blocksMade, BbodySignal blk@Block {blockBody}) <-
    judgmentContext

  validateBodySize blk (pp ^. ppProtocolVersionL)

  validateBodyHash blk

  let bhSlot = blk ^. blockHeaderSlotL

  (firstSlot, curEpoch) <- liftSTS $ slotToEpochBoundary bhSlot

  let txs = blockBody ^. txSeqBlockBodyL

  ls' <-
    trans @(EraRule "LEDGERS" era) $
      TRC
        ( LedgersEnv bhSlot curEpoch pp account
        , ls
        , StrictSeq.fromStrict txs
        )

  validateExUnits @era txs $ pp ^. ppMaxBlockExUnitsL

  pure $ BbodyState ls' $ incrBlocks blk firstSlot (pp ^. ppDG) blocksMade

instance
  ( EraRule "BBODY" era ~ AlonzoBBODY era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" ShelleyBbodyPredFailure era
  , Embed (EraRule "LEDGERS" era) (AlonzoBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , AlonzoEraTxWits era
  , EraBlockBody era
  , AlonzoEraPParams era
  , AlonzoEraTx era
  ) =>
  STS (AlonzoBBODY era)
  where
  type State (AlonzoBBODY era) = ShelleyBbodyState era

  type Signal (AlonzoBBODY era) = BbodySignal era

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
