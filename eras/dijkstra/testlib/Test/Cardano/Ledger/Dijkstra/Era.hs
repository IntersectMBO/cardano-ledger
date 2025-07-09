{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Era (
  module Test.Cardano.Ledger.Conway.Era,
  DijkstraEraTest,
) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Test.Cardano.Ledger.Conway.Era
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()

class
  ConwayEraTest era =>
  DijkstraEraTest era

instance EraTest DijkstraEra

instance ShelleyEraTest DijkstraEra

instance AllegraEraTest DijkstraEra

instance MaryEraTest DijkstraEra

instance AlonzoEraTest DijkstraEra

instance BabbageEraTest DijkstraEra

instance ConwayEraTest DijkstraEra

instance DijkstraEraTest DijkstraEra
