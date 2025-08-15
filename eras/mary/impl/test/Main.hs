{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Mary (MaryEra)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import qualified Test.Cardano.Ledger.Mary.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Mary.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Mary.Era.Spec (maryEraSpec)
import qualified Test.Cardano.Ledger.Mary.Imp as Imp
import Test.Cardano.Ledger.Mary.ImpTest ()
import qualified Test.Cardano.Ledger.Mary.ValueSpec as ValueSpec
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

main :: IO ()
main =
  ledgerTestMain $ do
    maryEraSpec @MaryEra
    describe "Mary" $ do
      ValueSpec.spec
      BinarySpec.spec
      CddlSpec.spec
      describe "Imp" $ do
        Imp.spec @MaryEra
      roundTripJsonEraSpec @MaryEra
      roundTripJsonShelleyEraSpec @MaryEra
