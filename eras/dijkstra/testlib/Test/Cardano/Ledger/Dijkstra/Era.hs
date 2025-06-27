{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Era (
  module Test.Cardano.Ledger.Conway.Era,
) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Plutus (Language (..))
import Test.Cardano.Ledger.Conway.Era
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

instance EraTest DijkstraEra where
  validTxOut = alonzoValidTxOut

  -- TODO add PlutusV4?
  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV3]

  genPParams = alonzoGenPParams

instance ShelleyEraTest DijkstraEra

instance AllegraEraTest DijkstraEra

instance MaryEraTest DijkstraEra

instance AlonzoEraTest DijkstraEra

instance BabbageEraTest DijkstraEra

instance ConwayEraTest DijkstraEra
