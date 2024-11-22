{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Ledgers () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Ledger ()
import Cardano.Ledger.Alonzo.Rules.Utxo (AlonzoUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUtxowPredFailure)
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

type instance EraRuleFailure "LEDGERS" AlonzoEra = ShelleyLedgersPredFailure AlonzoEra

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure AlonzoEra

instance InjectRuleFailure "LEDGERS" ShelleyLedgerPredFailure AlonzoEra where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxowPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxowPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxoPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPpupPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxoPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AllegraUtxoPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelegsPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelplPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPoolPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelegPredFailure AlonzoEra where
  injectFailure = LedgerFailure . injectFailure
