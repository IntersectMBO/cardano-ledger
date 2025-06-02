{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules ()
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Spec as ConwaySpec
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.Binary.RoundTrip ()
import Test.Cardano.Ledger.Dijkstra.ImpTest ()

main :: IO ()
main =
  ledgerTestMain $
    describe "Dijkstra" $ do
      ConwaySpec.spec @DijkstraEra
