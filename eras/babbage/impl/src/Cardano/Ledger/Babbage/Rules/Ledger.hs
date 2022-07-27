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
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPred)
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
  ( DELEGS,
    DelegsEnv (..),
    DelegsEvent,
    DelegsPredicateFailure,
  )
import Cardano.Ledger.Shelley.Rules.Ledger
  ( LedgerEnv (..),
    LedgerEvent (..),
    LedgerPredicateFailure (..),
  )
import qualified Cardano.Ledger.Shelley.Rules.Ledgers as Shelley
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
    Show (PParams era),
    Show (TxOut era),
    Show (Tx era),
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
  type PredicateFailure (BabbageLEDGER era) = LedgerPredicateFailure era
  type Event (BabbageLEDGER era) = LedgerEvent era

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
    STS (DELEGS era),
    PredicateFailure (EraRule "DELEGS" era) ~ DelegsPredicateFailure era,
    Event (EraRule "DELEGS" era) ~ DelegsEvent era
  ) =>
  Embed (DELEGS era) (BabbageLEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Era era,
    STS (BabbageUTXOW era),
    Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era,
    PredicateFailure (EraRule "UTXOW" era) ~ BabbageUtxowPred era
  ) =>
  Embed (BabbageUTXOW era) (BabbageLEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

instance
  ( Era era,
    STS (BabbageLEDGER era),
    PredicateFailure (EraRule "LEDGER" era) ~ LedgerPredicateFailure era,
    Event (EraRule "LEDGER" era) ~ LedgerEvent era
  ) =>
  Embed (BabbageLEDGER era) (Shelley.LEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
