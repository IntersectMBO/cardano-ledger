{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Allegra (AllegraEra)
import qualified Test.Cardano.Ledger.Allegra.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Allegra.Binary.Golden as Golden
import qualified Test.Cardano.Ledger.Allegra.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Allegra.Imp as Imp
import Test.Cardano.Ledger.Allegra.ImpTest ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

main :: IO ()
main =
  ledgerTestMain $
    describe "Allegra" $ do
      BinarySpec.spec
      CddlSpec.spec
      describe "Imp" $ do
        Imp.spec @AllegraEra
      roundTripJsonEraSpec @AllegraEra
      roundTripJsonShelleyEraSpec @AllegraEra
      Golden.spec @AllegraEra
