{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babbage (Babbage)
import qualified Test.Cardano.Ledger.Allegra.Imp as AllegraImp
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import qualified Test.Cardano.Ledger.Babbage.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Babbage.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Babbage.ImpTest ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

main :: IO ()
main =
  ledgerTestMain $
    describe "Babbage" $ do
      BinarySpec.spec
      CddlSpec.spec
      roundTripJsonEraSpec @Babbage
      describe "Imp" $ do
        AllegraImp.spec @Babbage
        AlonzoImp.spec @Babbage
        ShelleyImp.spec @Babbage
      describe "CostModels" $ do
        CostModelsSpec.spec @Babbage
      describe "TxWits" $ do
        TxWitsSpec.spec @Babbage
