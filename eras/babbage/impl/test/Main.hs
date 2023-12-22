{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Core (PParams)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Babbage.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Babbage.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Babbage.ImpTest ()
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Core.JSON as JSON
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

main :: IO ()
main =
  ledgerTestMain $
    describe "Babbage" $ do
      BinarySpec.spec
      CddlSpec.spec
      describe "JSON" $ do
        JSON.roundTripEraSpec @(PParams Babbage)
      describe "Imp" $ do
        ShelleyImp.spec @Babbage
      describe "CostModels" $ do
        CostModelsSpec.spec @Babbage
