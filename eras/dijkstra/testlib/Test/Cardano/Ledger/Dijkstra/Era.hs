{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Cardano.Ledger.Dijkstra.Era () where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Test.Cardano.Ledger.Conway.Era (
  AllegraEraTest,
  AlonzoEraTest,
  BabbageEraTest,
  ConwayEraTest,
  EraTest,
  MaryEraTest,
  ShelleyEraTest,
 )

instance EraTest DijkstraEra

instance ShelleyEraTest DijkstraEra

instance AllegraEraTest DijkstraEra

instance MaryEraTest DijkstraEra

instance AlonzoEraTest DijkstraEra

instance BabbageEraTest DijkstraEra

instance ConwayEraTest DijkstraEra
