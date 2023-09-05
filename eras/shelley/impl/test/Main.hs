module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.BinarySpec as Binary

main :: IO ()
main =
  ledgerTestMain $
    describe "Shelley" $ do
      Binary.spec
