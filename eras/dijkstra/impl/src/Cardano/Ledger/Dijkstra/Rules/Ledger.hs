{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Ledger () where

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
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayLedgerEvent,
  ConwayLedgerPredFailure (..),
  ConwayUtxoPredFailure,
  ConwayUtxosPredFailure,
  ConwayUtxowPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.Utxow ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

type instance EraRuleFailure "LEDGER" DijkstraEra = ConwayLedgerPredFailure DijkstraEra

type instance EraRuleEvent "LEDGER" DijkstraEra = ConwayLedgerEvent DijkstraEra

instance InjectRuleFailure "LEDGER" ConwayLedgerPredFailure DijkstraEra

instance InjectRuleFailure "LEDGER" ConwayUtxowPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure

instance InjectRuleFailure "LEDGER" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayCertsPredFailure DijkstraEra where
  injectFailure = ConwayCertsFailure

instance InjectRuleFailure "LEDGER" ConwayCertPredFailure DijkstraEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayDelegPredFailure DijkstraEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovPredFailure DijkstraEra where
  injectFailure = ConwayGovFailure
