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
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

type instance EraRuleFailure "LEDGERS" (ConwayEra c) = ShelleyLedgersPredFailure (ConwayEra c)

type instance EraRuleEvent "LEDGERS" (ConwayEra c) = ShelleyLedgersEvent (ConwayEra c)

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure (ConwayEra c)

instance InjectRuleFailure "LEDGERS" ConwayLedgerPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertsPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovCertPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure
