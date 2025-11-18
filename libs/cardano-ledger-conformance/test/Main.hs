module Main (main) where

import qualified Test.Cardano.Ledger.Conformance.ConformanceSpec as ConformanceSpec
import Test.Cardano.Ledger.Common (describe , ledgerTestMain)
import qualified Test.Cardano.Ledger.Conformance.Spec.Conway as SpecConway
import qualified Test.Cardano.Ledger.Conformance.Imp.Conway as ImpConway

main :: IO ()
main =
  ledgerTestMain $ do
    describe "Conformance" $ do
      describe "Spec" $ ConformanceSpec.spec
      SpecConway.spec
      ImpConway.spec
