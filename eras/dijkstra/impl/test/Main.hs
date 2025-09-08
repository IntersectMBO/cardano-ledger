{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.Binary.RoundTrip ()
import qualified Test.Cardano.Ledger.Dijkstra.GoldenSpec as GoldenSpec
import qualified Test.Cardano.Ledger.Dijkstra.Imp as Imp
import Test.Cardano.Ledger.Dijkstra.ImpTest ()
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)
import qualified Test.Cardano.Ledger.Babbage.TxInfoSpec as BabbageTxInfo

main :: IO ()
main =
  ledgerTestMain $
    describe "Dijkstra" $ do
      GoldenSpec.spec
      roundTripJsonShelleyEraSpec @DijkstraEra
      describe "Imp" $ do
        Imp.spec @DijkstraEra
      describe "TxInfo" $ do
        BabbageTxInfo.spec @DijkstraEra
