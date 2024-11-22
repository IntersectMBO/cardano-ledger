{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Ledgers () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules.Ledger ()
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPredFailure)
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure,
  ShelleyDelplPredFailure,
  ShelleyLedgerPredFailure,
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

type instance EraRuleFailure "LEDGERS" BabbageEra = ShelleyLedgersPredFailure BabbageEra

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure BabbageEra

instance InjectRuleFailure "LEDGERS" ShelleyLedgerPredFailure BabbageEra where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxowPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxowPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxowPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxoPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxoPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPpupPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxoPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AllegraUtxoPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelegsPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelplPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPoolPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelegPredFailure BabbageEra where
  injectFailure = LedgerFailure . injectFailure
