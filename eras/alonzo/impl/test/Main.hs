{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Alonzo.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Alonzo.GoldenSpec as Golden
import qualified Test.Cardano.Ledger.Alonzo.GoldenTranslation as GoldenTranslation
import qualified Test.Cardano.Ledger.Alonzo.Imp as Imp
import Test.Cardano.Ledger.Alonzo.ImpTest ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

main :: IO ()
main =
  ledgerTestMain $
    describe "Alonzo" $ do
      BinarySpec.spec
      CddlSpec.spec
      roundTripJsonEraSpec @AlonzoEra
      roundTripJsonShelleyEraSpec @AlonzoEra
      GoldenTranslation.tests
      Golden.spec
      describe "Imp" $ do
        Imp.spec @AlonzoEra
      describe "CostModels" $ do
        CostModelsSpec.spec @AlonzoEra
      describe "TxWits" $ do
        TxWitsSpec.spec @AlonzoEra
