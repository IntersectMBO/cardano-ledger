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

import qualified Cardano.Ledger.Allegra.Rules as Allegra
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
import qualified Cardano.Ledger.Shelley.Rules as Shelley
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

type instance EraRuleFailure "LEDGER" AlonzoEra = Shelley.ShelleyLedgerPredFailure AlonzoEra

instance InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure AlonzoEra

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure AlonzoEra where
  injectFailure = Shelley.UtxowFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxowPredFailure AlonzoEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure AlonzoEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure AlonzoEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPpupPredFailure AlonzoEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxoPredFailure AlonzoEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Allegra.AllegraUtxoPredFailure AlonzoEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegsPredFailure AlonzoEra where
  injectFailure = Shelley.DelegsFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelplPredFailure AlonzoEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPoolPredFailure AlonzoEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegPredFailure AlonzoEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

-- =======================================

-- | An abstract Alonzo Era, Ledger transition. Fix 'someLedger' at a concrete type to
--   make it concrete.
ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( STS (someLEDGER era)
  , BaseM (someLEDGER era) ~ ShelleyBase
  , Signal (someLEDGER era) ~ StAnnTx TopTx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ Shelley.LedgerEnv era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "DELEGS" era) (someLEDGER era)
  , Environment (EraRule "DELEGS" era) ~ Shelley.DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , Environment (EraRule "UTXOW" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ StAnnTx TopTx era
  , AlonzoEraTx era
  , EraCertState era
  , EraRule "LEDGER" era ~ someLEDGER era
  , InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (Shelley.LedgerEnv slot mbCurEpochNo txIx pp account, LedgerState utxoSt certState, stAnnTx) <-
    judgmentContext
  let tx = stAnnTx ^. txStAnnTxG
      txBody = tx ^. bodyTxL

  curEpochNo <- maybe (liftSTS $ epochFromSlot slot) pure mbCurEpochNo

  certState' <-
    if tx ^. isValidTxL == IsValid True
      then do
        let withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
        Shelley.testIncompleteAndMissingWithdrawals (certState ^. certDStateL . accountsL) withdrawals
        trans @(EraRule "DELEGS" era) $
          TRC
            ( Shelley.DelegsEnv slot curEpochNo txIx pp tx account
            , certState & certDStateL . accountsL %~ drainAccounts withdrawals
            , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
            )
      else pure certState

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( Shelley.UtxoEnv @era slot pp certState
        , utxoSt
        , stAnnTx
        )
  pure $ LedgerState utxoSt' certState'

instance
  ( AlonzoEraTx era
  , EraGov era
  , Embed (EraRule "DELEGS" era) (AlonzoLEDGER era)
  , Embed (EraRule "UTXOW" era) (AlonzoLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ StAnnTx TopTx era
  , Environment (EraRule "DELEGS" era) ~ Shelley.DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , AtMostEra "Babbage" era
  , EraRule "LEDGER" era ~ AlonzoLEDGER era
  , EraRuleFailure "LEDGER" era ~ Shelley.ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure era
  , EraCertState era
  ) =>
  STS (AlonzoLEDGER era)
  where
  type State (AlonzoLEDGER era) = LedgerState era
  type Signal (AlonzoLEDGER era) = StAnnTx TopTx era
  type Environment (AlonzoLEDGER era) = Shelley.LedgerEnv era
  type BaseM (AlonzoLEDGER era) = ShelleyBase
  type PredicateFailure (AlonzoLEDGER era) = Shelley.ShelleyLedgerPredFailure era
  type Event (AlonzoLEDGER era) = Shelley.ShelleyLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @AlonzoLEDGER]

  renderAssertionViolation = Shelley.renderDepositEqualsObligationViolation

  assertions = Shelley.shelleyLedgerAssertions

instance
  ( Era era
  , STS (Shelley.DELEGS era)
  , PredicateFailure (EraRule "DELEGS" era) ~ Shelley.ShelleyDelegsPredFailure era
  , Event (EraRule "DELEGS" era) ~ Shelley.ShelleyDelegsEvent era
  ) =>
  Embed (Shelley.DELEGS era) (AlonzoLEDGER era)
  where
  wrapFailed = Shelley.DelegsFailure
  wrapEvent = Shelley.DelegsEvent

instance
  ( Era era
  , STS (AlonzoUTXOW era)
  , PredicateFailure (EraRule "UTXOW" era) ~ AlonzoUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (AlonzoUTXOW era) (AlonzoLEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
  wrapEvent = Shelley.UtxowEvent

instance
  ( Era era
  , STS (AlonzoLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerEvent era
  ) =>
  Embed (AlonzoLEDGER era) (Shelley.LEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
