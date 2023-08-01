module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Babbage.BinarySpec as BinarySpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Babbage" $ do
      BinarySpec.spec
