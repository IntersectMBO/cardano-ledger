{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babbage (BabbageEra)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Babbage.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Babbage.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Babbage.GoldenSpec as Golden
import qualified Test.Cardano.Ledger.Babbage.GoldenTranslation as GoldenTranslation
import qualified Test.Cardano.Ledger.Babbage.Imp as Imp
import Test.Cardano.Ledger.Babbage.ImpTest ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)

main :: IO ()
main =
  ledgerTestMain $
    describe "Babbage" $ do
      GoldenTranslation.spec
      Golden.spec
      BinarySpec.spec
      CddlSpec.spec
      roundTripJsonEraSpec @BabbageEra
      describe "Imp" $ do
        Imp.spec @BabbageEra
      describe "CostModels" $ do
        CostModelsSpec.spec @BabbageEra
      describe "TxWits" $ do
        TxWitsSpec.spec @BabbageEra
