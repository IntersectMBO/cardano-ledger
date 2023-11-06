module Main where

import qualified Test.Cardano.Ledger.Conformance as Conformance
import Test.Cardano.Ledger.Imp.Common (describe, ledgerTestMain)

main :: IO ()
main =
  ledgerTestMain $
    describe "Conformance" $ do
      Conformance.spec
