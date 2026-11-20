{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Ledgers () where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "LEDGERS" DijkstraEra = Conway.ConwayLedgersPredFailure DijkstraEra

type instance EraRuleEvent "LEDGERS" DijkstraEra = Shelley.ShelleyLedgersEvent DijkstraEra

instance InjectRuleFailure "LEDGERS" Conway.ConwayLedgersPredFailure DijkstraEra

instance InjectRuleFailure "LEDGERS" DijkstraLedgerPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers

instance InjectRuleFailure "LEDGERS" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxowPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxoPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayUtxosPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayGovPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraGovPredFailure DijkstraEra where
  injectFailure = Conway.LedgerFailureInLedgers . injectFailure
