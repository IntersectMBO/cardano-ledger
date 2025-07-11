{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules ()
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Spec as ConwaySpec
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.Binary.RoundTrip ()
import qualified Test.Cardano.Ledger.Dijkstra.GoldenSpec as GoldenSpec
import Test.Cardano.Ledger.Dijkstra.ImpTest ()
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

main :: IO ()
main =
  ledgerTestMain $ do
    describe "Dijkstra" $ do
      ConwaySpec.spec @DijkstraEra
    roundTripJsonShelleyEraSpec @DijkstraEra
    GoldenSpec.spec
