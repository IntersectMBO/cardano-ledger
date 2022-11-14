{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Ledger (BabbageLEDGER) where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxowEvent, ledgerTransition)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, AlonzoTx (..))
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    LedgerState (..),
    PState (..),
    UTxOState (..),
    rewards,
  )
import Cardano.Ledger.Shelley.Rules.Delegs
  ( DelegsEnv (..),
    ShelleyDELEGS,
    ShelleyDelegsEvent,
    ShelleyDelegsPredFailure,
  )
import Cardano.Ledger.Shelley.Rules.Ledger
  ( LedgerEnv (..),
    ShelleyLedgerEvent (..),
    ShelleyLedgerPredFailure (..),
  )
import Cardano.Ledger.Shelley.Rules.Ledgers (ShelleyLEDGERS)
import Cardano.Ledger.Shelley.Rules.Ledgers as Shelley
  ( ShelleyLedgersEvent (LedgerEvent),
    ShelleyLedgersPredFailure (LedgerFailure),
  )
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.TxBody (DCert)
import Control.State.Transition
  ( Assertion (..),
    AssertionViolation (..),
    Embed (..),
    STS (..),
    TRC (..),
  )
import Data.Sequence (Seq)
import GHC.Records (HasField)

-- ==================================================

instance
  ( AlonzoEraTx era,
    Tx era ~ AlonzoTx era,
    Show (State (EraRule "PPUP" era)),
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    Embed (EraRule "DELEGS" era) (BabbageLEDGER era),
    Embed (EraRule "UTXOW" era) (BabbageLEDGER era),
    Environment (EraRule "UTXOW" era) ~ UtxoEnv era,
    State (EraRule "UTXOW" era) ~ UTxOState era,
    Signal (EraRule "UTXOW" era) ~ AlonzoTx era,
    Environment (EraRule "DELEGS" era) ~ DelegsEnv era,
    State (EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (EraRule "DELEGS" era) ~ Seq (DCert (Crypto era))
  ) =>
  STS (BabbageLEDGER era)
  where
  type State (BabbageLEDGER era) = LedgerState era
  type Signal (BabbageLEDGER era) = AlonzoTx era
  type Environment (BabbageLEDGER era) = LedgerEnv era
  type BaseM (BabbageLEDGER era) = ShelleyBase
  type PredicateFailure (BabbageLEDGER era) = ShelleyLedgerPredFailure era
  type Event (BabbageLEDGER era) = ShelleyLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @BabbageLEDGER]

  renderAssertionViolation AssertionViolation {avSTS, avMsg, avCtx, avState} =
    "AssertionViolation (" <> avSTS <> "): " <> avMsg
      <> "\n"
      <> show avCtx
      <> "\n"
      <> show avState

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation"
        ( \(TRC (LedgerEnv {ledgerPp}, _, _))
           (LedgerState utxoSt DPState {dpsDState, dpsPState}) ->
              obligation ledgerPp (rewards dpsDState) (_pParams dpsPState)
                == _deposited utxoSt
        )
    ]

instance
  ( Era era,
    STS (ShelleyDELEGS era),
    PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era,
    Event (EraRule "DELEGS" era) ~ ShelleyDelegsEvent era
  ) =>
  Embed (ShelleyDELEGS era) (BabbageLEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Era era,
    STS (BabbageUTXOW era),
    Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era,
    PredicateFailure (EraRule "UTXOW" era) ~ BabbageUtxowPredFailure era
  ) =>
  Embed (BabbageUTXOW era) (BabbageLEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

instance
  ( Era era,
    STS (BabbageLEDGER era),
    PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era,
    Event (EraRule "LEDGER" era) ~ ShelleyLedgerEvent era
  ) =>
  Embed (BabbageLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
