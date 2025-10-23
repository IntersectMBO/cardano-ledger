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

module Cardano.Ledger.Babbage.Rules.Ledger (BabbageLEDGER) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent,
  AlonzoUtxowPredFailure,
  ledgerTransition,
 )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra, BabbageLEDGER)
import Cardano.Ledger.Babbage.Rules.Delegs ()
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPredFailure)
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUTXOW, BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules (
  DelegsEnv (..),
  LedgerEnv (..),
  ShelleyDELEGS,
  ShelleyDelegPredFailure,
  ShelleyDelegsEvent,
  ShelleyDelegsPredFailure,
  ShelleyDelplPredFailure,
  ShelleyLEDGERS,
  ShelleyLedgerEvent (..),
  ShelleyLedgerPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
  shelleyLedgerAssertions,
 )
import Cardano.Ledger.Shelley.Rules as Shelley (
  ShelleyLedgersEvent (LedgerEvent),
  ShelleyLedgersPredFailure (LedgerFailure),
  renderDepositEqualsObligationViolation,
 )
import Cardano.Ledger.State (EraCertState)
import Control.State.Transition (
  Embed (..),
  STS (..),
 )
import Data.Sequence (Seq)

-- ==================================================

type instance EraRuleFailure "LEDGER" BabbageEra = ShelleyLedgerPredFailure BabbageEra

instance InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure BabbageEra

instance InjectRuleFailure "LEDGER" BabbageUtxowPredFailure BabbageEra where
  injectFailure = UtxowFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure BabbageEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure BabbageEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure BabbageEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure BabbageEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure BabbageEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPpupPredFailure BabbageEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure BabbageEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AllegraUtxoPredFailure BabbageEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegsPredFailure BabbageEra where
  injectFailure = DelegsFailure

instance InjectRuleFailure "LEDGER" ShelleyDelplPredFailure BabbageEra where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure BabbageEra where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure BabbageEra where
  injectFailure = DelegsFailure . injectFailure

instance
  ( AlonzoEraTx era
  , EraGov era
  , Embed (EraRule "DELEGS" era) (BabbageLEDGER era)
  , Embed (EraRule "UTXOW" era) (BabbageLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , AtMostEra "Babbage" era
  , EraCertState era
  , EraRule "LEDGER" era ~ BabbageLEDGER era
  , EraRuleFailure "LEDGER" era ~ ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  ) =>
  STS (BabbageLEDGER era)
  where
  type State (BabbageLEDGER era) = LedgerState era
  type Signal (BabbageLEDGER era) = Tx era
  type Environment (BabbageLEDGER era) = LedgerEnv era
  type BaseM (BabbageLEDGER era) = ShelleyBase
  type PredicateFailure (BabbageLEDGER era) = ShelleyLedgerPredFailure era
  type Event (BabbageLEDGER era) = ShelleyLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @BabbageLEDGER]

  renderAssertionViolation = Shelley.renderDepositEqualsObligationViolation

  assertions = shelleyLedgerAssertions

instance
  ( Era era
  , STS (ShelleyDELEGS era)
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  , Event (EraRule "DELEGS" era) ~ ShelleyDelegsEvent era
  ) =>
  Embed (ShelleyDELEGS era) (BabbageLEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Era era
  , STS (BabbageUTXOW era)
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  , PredicateFailure (EraRule "UTXOW" era) ~ BabbageUtxowPredFailure era
  ) =>
  Embed (BabbageUTXOW era) (BabbageLEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

instance
  ( Era era
  , STS (BabbageLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ShelleyLedgerEvent era
  ) =>
  Embed (BabbageLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
