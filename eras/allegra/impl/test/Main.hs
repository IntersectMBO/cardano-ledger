module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Allegra.BinarySpec as BinarySpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Allegra" $ do
      BinarySpec.spec
