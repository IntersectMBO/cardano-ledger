{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Era () where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.State
import Data.Coerce
import Test.Cardano.Ledger.Conway.Era (
  AllegraEraTest,
  AlonzoEraTest,
  BabbageEraTest,
  ConwayEraTest,
  EraTest (..),
  MaryEraTest,
  ShelleyEraTest,
  conwayAccountsToUMap,
  mkConwayTestAccountState,
 )
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()

instance EraTest DijkstraEra where
  mkTestAccountState = mkConwayTestAccountState

  accountsFromAccountsMap = coerce

  accountsToUMap = conwayAccountsToUMap

instance ShelleyEraTest DijkstraEra

instance AllegraEraTest DijkstraEra

instance MaryEraTest DijkstraEra

instance AlonzoEraTest DijkstraEra

instance BabbageEraTest DijkstraEra

instance ConwayEraTest DijkstraEra
