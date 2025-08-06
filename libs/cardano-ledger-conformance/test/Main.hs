{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conformance.ConformanceSpec as ConformanceSpec
import qualified Test.Cardano.Ledger.Conformance.Spec.Conway as Conway

main :: IO ()
main =
  ledgerTestMain $ do
    describe "ConformanceSpec" $ ConformanceSpec.spec
    describe "Conway" $ do
      describe "Imp" Conway.spec
