{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CanonicalSpec as Canonical
import qualified Test.Cardano.Ledger.Alonzo.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Alonzo.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Alonzo.Era.Spec (alonzoEraSpec)
import qualified Test.Cardano.Ledger.Alonzo.GoldenSpec as Golden
import qualified Test.Cardano.Ledger.Alonzo.GoldenTranslation as GoldenTranslation
import qualified Test.Cardano.Ledger.Alonzo.Imp as Imp
import Test.Cardano.Ledger.Alonzo.ImpTest ()
import qualified Test.Cardano.Ledger.Alonzo.TxInfoSpec as TxInfo
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

main :: IO ()
main =
  ledgerTestMain $ do
    alonzoEraSpec @AlonzoEra
    describe "Alonzo" $ do
      BinarySpec.spec
      Canonical.spec
      CddlSpec.spec
      roundTripJsonEraSpec @AlonzoEra
      roundTripJsonShelleyEraSpec @AlonzoEra
      TxInfo.spec
      GoldenTranslation.tests
      Golden.spec
      describe "Imp" $ do
        Imp.spec @AlonzoEra
      describe "CostModels" $ do
        CostModelsSpec.spec @AlonzoEra
      describe "TxWits" $ do
        TxWitsSpec.spec @AlonzoEra
