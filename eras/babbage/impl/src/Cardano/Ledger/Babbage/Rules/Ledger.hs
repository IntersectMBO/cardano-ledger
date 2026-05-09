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

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
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
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (EraCertState)
import Control.State.Transition (
  Embed (..),
  STS (..),
 )
import Data.Sequence (Seq)

-- ==================================================

type instance EraRuleFailure "LEDGER" BabbageEra = Shelley.ShelleyLedgerPredFailure BabbageEra

instance InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure BabbageEra

instance InjectRuleFailure "LEDGER" BabbageUtxowPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure

instance InjectRuleFailure "LEDGER" Alonzo.AlonzoUtxowPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxowPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Alonzo.AlonzoUtxoPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Alonzo.AlonzoUtxosPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPpupPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxoPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Allegra.AllegraUtxoPredFailure BabbageEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegsPredFailure BabbageEra where
  injectFailure = Shelley.DelegsFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelplPredFailure BabbageEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPoolPredFailure BabbageEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegPredFailure BabbageEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance
  ( AlonzoEraTx era
  , EraGov era
  , Embed (EraRule "DELEGS" era) (BabbageLEDGER era)
  , Embed (EraRule "UTXOW" era) (BabbageLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ StAnnTx TopTx era
  , Environment (EraRule "DELEGS" era) ~ Shelley.DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , AtMostEra "Babbage" era
  , EraCertState era
  , EraRule "LEDGER" era ~ BabbageLEDGER era
  , EraRuleFailure "LEDGER" era ~ Shelley.ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure era
  ) =>
  STS (BabbageLEDGER era)
  where
  type State (BabbageLEDGER era) = LedgerState era
  type Signal (BabbageLEDGER era) = StAnnTx TopTx era
  type Environment (BabbageLEDGER era) = Shelley.LedgerEnv era
  type BaseM (BabbageLEDGER era) = ShelleyBase
  type PredicateFailure (BabbageLEDGER era) = Shelley.ShelleyLedgerPredFailure era
  type Event (BabbageLEDGER era) = Shelley.ShelleyLedgerEvent era

  initialRules = []
  transitionRules = [Alonzo.ledgerTransition @BabbageLEDGER]

  renderAssertionViolation = Shelley.renderDepositEqualsObligationViolation

  assertions = Shelley.shelleyLedgerAssertions

instance
  ( Era era
  , STS (Shelley.ShelleyDELEGS era)
  , PredicateFailure (EraRule "DELEGS" era) ~ Shelley.ShelleyDelegsPredFailure era
  , Event (EraRule "DELEGS" era) ~ Shelley.ShelleyDelegsEvent era
  ) =>
  Embed (Shelley.ShelleyDELEGS era) (BabbageLEDGER era)
  where
  wrapFailed = Shelley.DelegsFailure
  wrapEvent = Shelley.DelegsEvent

instance
  ( Era era
  , STS (BabbageUTXOW era)
  , Event (EraRule "UTXOW" era) ~ Alonzo.AlonzoUtxowEvent era
  , PredicateFailure (EraRule "UTXOW" era) ~ BabbageUtxowPredFailure era
  ) =>
  Embed (BabbageUTXOW era) (BabbageLEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
  wrapEvent = Shelley.UtxowEvent

instance
  ( Era era
  , STS (BabbageLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerEvent era
  ) =>
  Embed (BabbageLEDGER era) (Shelley.ShelleyLEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
