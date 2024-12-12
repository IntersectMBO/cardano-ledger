{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Ledgers () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

type instance EraRuleFailure "LEDGERS" ConwayEra = ShelleyLedgersPredFailure ConwayEra

type instance EraRuleEvent "LEDGERS" ConwayEra = ShelleyLedgersEvent ConwayEra

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure ConwayEra

instance InjectRuleFailure "LEDGERS" ConwayLedgerPredFailure ConwayEra where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxowPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxowPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxowPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxowPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxosPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AllegraUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertsPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayDelegPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPoolPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovCertPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovPredFailure ConwayEra where
  injectFailure = LedgerFailure . injectFailure
