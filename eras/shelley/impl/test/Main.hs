{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Shelley (Shelley)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import qualified Test.Cardano.Ledger.Shelley.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Shelley.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Shelley.Imp as Imp

main :: IO ()
main =
  ledgerTestMain $
    describe "Shelley" $ do
      Binary.spec
      Cddl.spec
      Imp.spec @Shelley
      roundTripJsonEraSpec @Shelley
