module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.Serialization.GoldenSpec as GoldenSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Mary" $ do
      GoldenSpec.spec
