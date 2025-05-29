{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Bbody () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoBbodyEvent,
  AlonzoBbodyPredFailure,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Conway.Rules (
  ConwayBbodyPredFailure,
  ConwayCertPredFailure,
  ConwayCertsPredFailure,
  ConwayDelegPredFailure,
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayLedgerPredFailure,
  ConwayUtxoPredFailure,
  ConwayUtxosPredFailure,
  ConwayUtxowPredFailure,
  alonzoToConwayBbodyPredFailure,
  shelleyToConwayBbodyPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyPredFailure,
  ShelleyLedgersPredFailure,
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "BBODY" DijkstraEra = ConwayBbodyPredFailure DijkstraEra

type instance EraRuleEvent "BBODY" DijkstraEra = AlonzoBbodyEvent DijkstraEra

instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure DijkstraEra

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure DijkstraEra where
  injectFailure = alonzoToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" ConwayLedgerPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertsPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayDelegPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovPredFailure DijkstraEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure
