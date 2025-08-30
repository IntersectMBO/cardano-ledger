{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babbage (BabbageEra)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Babbage.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Babbage.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Babbage.Era.Spec (babbageEraSpec)
import qualified Test.Cardano.Ledger.Babbage.GoldenSpec as Golden
import qualified Test.Cardano.Ledger.Babbage.GoldenTranslation as GoldenTranslation
import qualified Test.Cardano.Ledger.Babbage.Imp as Imp
import Test.Cardano.Ledger.Babbage.ImpTest ()
import qualified Test.Cardano.Ledger.Babbage.TxInfoSpec as TxInfo
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

main :: IO ()
main =
  ledgerTestMain $ do
    babbageEraSpec @BabbageEra
    describe "Babbage" $ do
      TxInfo.spec @BabbageEra
      GoldenTranslation.spec
      Golden.spec
      BinarySpec.spec
      CddlSpec.spec
      roundTripJsonEraSpec @BabbageEra
      roundTripJsonShelleyEraSpec @BabbageEra
      describe "Imp" $ do
        Imp.spec @BabbageEra
      describe "CostModels" $ do
        CostModelsSpec.spec @BabbageEra
      describe "TxWits" $ do
        TxWitsSpec.spec @BabbageEra
