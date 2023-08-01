module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Alonzo.BinarySpec as BinarySpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Alonzo" $ do
      BinarySpec.spec
