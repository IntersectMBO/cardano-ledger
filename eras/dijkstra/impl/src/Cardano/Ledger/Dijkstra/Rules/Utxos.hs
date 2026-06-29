{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxos () where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosEvent, AlonzoUtxosPredFailure)
import Cardano.Ledger.Conway.Rules (
  ConwayUtxosEvent,
  ConwayUtxosPredFailure,
  alonzoToConwayUtxosEvent,
  alonzoToConwayUtxosPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core (
  EraRuleEvent,
  EraRuleFailure,
  InjectRuleEvent (..),
  InjectRuleFailure (..),
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)

type instance EraRuleFailure "UTXOS" DijkstraEra = ConwayUtxosPredFailure DijkstraEra

type instance EraRuleEvent "UTXOS" DijkstraEra = ConwayUtxosEvent DijkstraEra

instance InjectRuleFailure "UTXOS" ConwayUtxosPredFailure DijkstraEra

instance InjectRuleEvent "UTXOS" ConwayUtxosEvent DijkstraEra

instance InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = alonzoToConwayUtxosPredFailure

instance InjectRuleEvent "UTXOS" AlonzoUtxosEvent DijkstraEra where
  injectEvent = alonzoToConwayUtxosEvent
