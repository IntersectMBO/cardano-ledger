{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CanonicalSpec as Canonical
import qualified Test.Cardano.Ledger.Alonzo.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Alonzo.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Alonzo.GoldenSpec as GoldenSpec
import qualified Test.Cardano.Ledger.Alonzo.GoldenTranslation as GoldenTranslation
import qualified Test.Cardano.Ledger.Alonzo.Imp as Imp
import qualified Test.Cardano.Ledger.Alonzo.Imp.TxInfoSpec as TxInfo
import Test.Cardano.Ledger.Alonzo.ImpTest ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

instance EraSpec AlonzoEra where
  eraImpSpec era = do
    Imp.shelleyToBabbageSpec era
    Imp.alonzoToConwaySpec era
    Imp.spec era

main :: IO ()
main =
  ledgerEraTestMain @AlonzoEra $ do
    BinarySpec.spec
    Canonical.spec
    CddlSpec.spec
    roundTripJsonEraSpec @AlonzoEra
    roundTripJsonShelleyEraSpec @AlonzoEra
    GoldenTranslation.tests
    GoldenSpec.spec
    describe "CostModels" $ do
      CostModelsSpec.spec @AlonzoEra
    describe "TxWits" $ do
      TxWitsSpec.spec @AlonzoEra
    TxInfo.spec
