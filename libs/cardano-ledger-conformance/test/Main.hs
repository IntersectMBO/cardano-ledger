module Main (main) where

import Test.Cardano.Ledger.Common (describe, ledgerTestMain)
import qualified Test.Cardano.Ledger.Conformance.Imp.Conway as ImpConway
import qualified Test.Cardano.Ledger.Conformance.Spec.Base as SpecBase
import qualified Test.Cardano.Ledger.Conformance.Spec.Conway as SpecConway

main :: IO ()
main =
  ledgerTestMain $ do
    describe "Conformance" $ do
      describe "Base" $ SpecBase.spec
      SpecConway.spec
      ImpConway.spec
