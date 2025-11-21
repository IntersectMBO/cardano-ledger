{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Ledgers () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure,
  ConwayCertsPredFailure,
  ConwayDelegPredFailure,
  ConwayGovPredFailure,
  ConwayUtxosPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

type instance EraRuleFailure "LEDGERS" DijkstraEra = ShelleyLedgersPredFailure DijkstraEra

type instance EraRuleEvent "LEDGERS" DijkstraEra = ShelleyLedgersEvent DijkstraEra

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure DijkstraEra

instance InjectRuleFailure "LEDGERS" DijkstraLedgerPredFailure DijkstraEra where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertsPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayDelegPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraGovPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure
