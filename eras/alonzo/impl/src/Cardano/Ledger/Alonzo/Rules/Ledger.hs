{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Ledger (
  AlonzoLEDGER,
  ledgerTransition,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Era (AlonzoEra, AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Rules.Delegs ()
import Cardano.Ledger.Alonzo.Rules.Utxo (AlonzoUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUTXOW, AlonzoUtxowEvent, AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules (
  DelegsEnv (..),
  ShelleyDELEGS,
  ShelleyDelegPredFailure,
  ShelleyDelegsEvent,
  ShelleyDelegsPredFailure,
  ShelleyDelplPredFailure (..),
  ShelleyLEDGERS,
  ShelleyPoolPredFailure,
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
  shelleyLedgerAssertions,
  testIncompleteAndMissingWithdrawals,
 )
import Cardano.Ledger.Shelley.Rules as Shelley (
  LedgerEnv (..),
  ShelleyLedgerEvent (..),
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersEvent (..),
  ShelleyLedgersPredFailure (..),
  renderDepositEqualsObligationViolation,
 )
import Cardano.Ledger.Slot (epochFromSlot)
import Cardano.Ledger.State (EraCertState, accountsL, certDStateL, drainAccounts)
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro

type instance EraRuleFailure "LEDGER" AlonzoEra = ShelleyLedgerPredFailure AlonzoEra

instance InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure AlonzoEra

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure AlonzoEra where
  injectFailure = UtxowFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure AlonzoEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure AlonzoEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure AlonzoEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPpupPredFailure AlonzoEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure AlonzoEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AllegraUtxoPredFailure AlonzoEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegsPredFailure AlonzoEra where
  injectFailure = DelegsFailure

instance InjectRuleFailure "LEDGER" ShelleyDelplPredFailure AlonzoEra where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure AlonzoEra where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure AlonzoEra where
  injectFailure = DelegsFailure . injectFailure

-- =======================================

-- | An abstract Alonzo Era, Ledger transition. Fix 'someLedger' at a concrete type to
--   make it concrete.
ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( STS (someLEDGER era)
  , BaseM (someLEDGER era) ~ ShelleyBase
  , Signal (someLEDGER era) ~ Tx TopTx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "DELEGS" era) (someLEDGER era)
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , AlonzoEraTx era
  , EraCertState era
  , EraRule "LEDGER" era ~ someLEDGER era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot mbCurEpochNo txIx pp account, LedgerState utxoSt certState, tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL

  curEpochNo <- maybe (liftSTS $ epochFromSlot slot) pure mbCurEpochNo

  certState' <-
    if tx ^. isValidTxL == IsValid True
      then do
        let withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
        testIncompleteAndMissingWithdrawals (certState ^. certDStateL . accountsL) withdrawals
        trans @(EraRule "DELEGS" era) $
          TRC
            ( DelegsEnv slot curEpochNo txIx pp tx account
            , certState & certDStateL . accountsL %~ drainAccounts withdrawals
            , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
            )
      else pure certState

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv @era slot pp certState
        , utxoSt
        , tx
        )
  pure $ LedgerState utxoSt' certState'

instance
  ( AlonzoEraTx era
  , EraGov era
  , Embed (EraRule "DELEGS" era) (AlonzoLEDGER era)
  , Embed (EraRule "UTXOW" era) (AlonzoLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , AtMostEra "Babbage" era
  , EraRule "LEDGER" era ~ AlonzoLEDGER era
  , EraRuleFailure "LEDGER" era ~ ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , EraCertState era
  ) =>
  STS (AlonzoLEDGER era)
  where
  type State (AlonzoLEDGER era) = LedgerState era
  type Signal (AlonzoLEDGER era) = Tx TopTx era
  type Environment (AlonzoLEDGER era) = LedgerEnv era
  type BaseM (AlonzoLEDGER era) = ShelleyBase
  type PredicateFailure (AlonzoLEDGER era) = ShelleyLedgerPredFailure era
  type Event (AlonzoLEDGER era) = ShelleyLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @AlonzoLEDGER]

  renderAssertionViolation = Shelley.renderDepositEqualsObligationViolation

  assertions = shelleyLedgerAssertions

instance
  ( Era era
  , STS (ShelleyDELEGS era)
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  , Event (EraRule "DELEGS" era) ~ ShelleyDelegsEvent era
  ) =>
  Embed (ShelleyDELEGS era) (AlonzoLEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Era era
  , STS (AlonzoUTXOW era)
  , PredicateFailure (EraRule "UTXOW" era) ~ AlonzoUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (AlonzoUTXOW era) (AlonzoLEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

instance
  ( Era era
  , STS (AlonzoLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ShelleyLedgerEvent era
  ) =>
  Embed (AlonzoLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
