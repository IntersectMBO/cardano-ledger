{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conformance.ConformanceSpec as ConformanceSpec
import qualified Test.Cardano.Ledger.Conformance.Spec.Conway as SpecConway
import qualified Test.Cardano.Ledger.Conformance.Imp.Conway as ImpConway

main :: IO ()
main =
  ledgerTestMain $ do
    describe "Conformance" $ do
      describe "Spec" $ ConformanceSpec.spec
      SpecConway.spec
      ImpConway.spec
