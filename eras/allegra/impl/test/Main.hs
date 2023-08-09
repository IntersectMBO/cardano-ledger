module Main where

import qualified Test.Cardano.Ledger.Allegra.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Common

main :: IO ()
main =
  ledgerTestMain $
    describe "Allegra" $ do
      BinarySpec.spec
