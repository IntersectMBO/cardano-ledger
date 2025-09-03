{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Shelley (ShelleyEra)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import qualified Test.Cardano.Ledger.Shelley.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Shelley.BinarySpec as Binary
import Test.Cardano.Ledger.Shelley.Era.Spec (shelleyEraSpec)
import qualified Test.Cardano.Ledger.Shelley.Imp as Imp
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

main :: IO ()
main =
  ledgerTestMain $ do
    shelleyEraSpec @ShelleyEra
    describe "Shelley" $ do
      Binary.spec
      Cddl.spec
      Imp.spec @ShelleyEra
      roundTripJsonEraSpec @ShelleyEra
      roundTripJsonShelleyEraSpec @ShelleyEra
