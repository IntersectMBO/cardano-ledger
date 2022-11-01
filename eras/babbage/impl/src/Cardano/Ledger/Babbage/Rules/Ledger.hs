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

module Cardano.Ledger.Babbage.Rules.Ledger (BabbageLEDGER) where

import Cardano.Ledger.Alonzo.Rules.Ledger (ledgerTransition)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoEvent)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Utxos (ConcreteBabbage)
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUTXOW, BabbageUtxowPred)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript)
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

-- ==================================================
data BabbageLEDGER c

instance
<<<<<<< HEAD
  ( Era era,
    ValidateScript era,
    ConcreteBabbage era,
    Show (State (Core.EraRule "PPUP" era)),
    Embed (Core.EraRule "DELEGS" era) (BabbageLEDGER era),
    Embed (Core.EraRule "UTXOW" era) (BabbageLEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ ValidatedTx era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Show (ValidatedTx era)
=======
  ( AlonzoEraTx era,
    Show (State (EraRule "PPUP" era)),
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    Embed (EraRule "DELEGS" era) (BabbageLEDGER era),
    Embed (EraRule "UTXOW" era) (BabbageLEDGER era),
    Environment (EraRule "UTXOW" era) ~ UtxoEnv era,
    State (EraRule "UTXOW" era) ~ UTxOState era,
    Signal (EraRule "UTXOW" era) ~ Tx era,
    Environment (EraRule "DELEGS" era) ~ DelegsEnv era,
    State (EraRule "DELEGS" era) ~ DPState (EraCrypto era),
    Signal (EraRule "DELEGS" era) ~ Seq (DCert (EraCrypto era))
>>>>>>> f63095744 (Fixes to compile on ghc-9.2.4)
  ) =>
  STS (BabbageLEDGER era)
  where
  type State (BabbageLEDGER era) = LedgerState era
  type Signal (BabbageLEDGER era) = ValidatedTx era
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
    PredicateFailure (Core.EraRule "DELEGS" era) ~ DelegsPredicateFailure era,
    Event (Core.EraRule "DELEGS" era) ~ DelegsEvent era
  ) =>
  Embed (DELEGS era) (BabbageLEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Era era,
    STS (BabbageUTXOW era),
    Event (Core.EraRule "UTXOW" era) ~ AlonzoEvent era,
    PredicateFailure (Core.EraRule "UTXOW" era) ~ BabbageUtxowPred era
  ) =>
  Embed (BabbageUTXOW era) (BabbageLEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

instance
  ( Era era,
    STS (BabbageLEDGER era),
    PredicateFailure (Core.EraRule "LEDGER" era) ~ LedgerPredicateFailure era,
    Event (Core.EraRule "LEDGER" era) ~ LedgerEvent era
  ) =>
  Embed (BabbageLEDGER era) (Shelley.LEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
