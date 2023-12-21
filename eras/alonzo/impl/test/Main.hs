{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Core (PParams)
import Data.Proxy (Proxy (Proxy))
import qualified Test.Cardano.Ledger.Alonzo.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Alonzo.ImpTest ()
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Core.JSON as JSON
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

main :: IO ()
main =
  ledgerTestMain $
    describe "Alonzo" $ do
      BinarySpec.spec
      CddlSpec.spec
      describe "JSON" $ do
        JSON.roundTripEraSpec @(PParams Alonzo)
      describe "Imp" $ do
        ShelleyImp.spec @Alonzo
      describe "CostModels" $ do
        CostModelsSpec.spec @Alonzo Proxy
