{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), AlonzoTx (..), IsValid (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
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
 )
import Cardano.Ledger.Shelley.Rules as Shelley (
  LedgerEnv (..),
  ShelleyLedgerEvent (..),
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersEvent (..),
  ShelleyLedgersPredFailure (..),
  renderDepositEqualsObligationViolation,
 )
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro

type instance EraRuleFailure "LEDGER" (AlonzoEra c) = ShelleyLedgerPredFailure (AlonzoEra c)

instance InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure (AlonzoEra c)

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure (AlonzoEra c) where
  injectFailure = UtxowFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure (AlonzoEra c) where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure (AlonzoEra c) where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure (AlonzoEra c) where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPpupPredFailure (AlonzoEra c) where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure (AlonzoEra c) where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AllegraUtxoPredFailure (AlonzoEra c) where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegsPredFailure (AlonzoEra c) where
  injectFailure = DelegsFailure

instance InjectRuleFailure "LEDGER" ShelleyDelplPredFailure (AlonzoEra c) where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure (AlonzoEra c) where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure (AlonzoEra c) where
  injectFailure = DelegsFailure . injectFailure

-- =======================================

-- | An abstract Alonzo Era, Ledger transition. Fix 'someLedger' at a concrete type to
--   make it concrete.
ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( Signal (someLEDGER era) ~ Tx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "DELEGS" era) (someLEDGER era)
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , AlonzoEraTx era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account _, LedgerState utxoSt certState, tx) <- judgmentContext
  let txBody = tx ^. bodyTxL

  certState' <-
    if tx ^. isValidTxL == IsValid True
      then
        trans @(EraRule "DELEGS" era) $
          TRC
            ( DelegsEnv slot txIx pp tx account
            , certState
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
  ( DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , AlonzoEraTx era
  , EraGov era
  , Tx era ~ AlonzoTx era
  , Embed (EraRule "DELEGS" era) (AlonzoLEDGER era)
  , Embed (EraRule "UTXOW" era) (AlonzoLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ AlonzoTx era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , ProtVerAtMost era 8
  ) =>
  STS (AlonzoLEDGER era)
  where
  type State (AlonzoLEDGER era) = LedgerState era
  type Signal (AlonzoLEDGER era) = AlonzoTx era
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
