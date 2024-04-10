module Main (main) where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conformance.Spec.Conway as Conway

main :: IO ()
main =
  ledgerTestMain $
    describe "Conformance" $ do
      Conway.spec
