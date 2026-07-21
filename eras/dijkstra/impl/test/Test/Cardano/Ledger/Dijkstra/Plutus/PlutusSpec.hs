{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Plutus.PlutusSpec (spec) where

import Cardano.Ledger.Core (PParamsUpdate)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Plutus.ToPlutusData (roundTripPlutusDataSpec)

spec :: Spec
spec = do
  describe "roundtrip ToPlutusData Dijkstra instances" $ do
    roundTripPlutusDataSpec @(PParamsUpdate DijkstraEra)
