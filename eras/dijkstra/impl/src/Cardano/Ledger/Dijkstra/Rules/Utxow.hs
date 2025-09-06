{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxow () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Conway.Rules (
  ConwayUtxoPredFailure,
  ConwayUtxosPredFailure,
  ConwayUtxowPredFailure (..),
  alonzoToConwayUtxowPredFailure,
  babbageToConwayUtxowPredFailure,
  shelleyToConwayUtxowPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Utxo ()
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)

type instance EraRuleFailure "UTXOW" DijkstraEra = ConwayUtxowPredFailure DijkstraEra

type instance EraRuleEvent "UTXOW" DijkstraEra = AlonzoUtxowEvent DijkstraEra

instance InjectRuleFailure "UTXOW" ConwayUtxowPredFailure DijkstraEra

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = babbageToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = alonzoToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure
