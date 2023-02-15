module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.Binary.GoldenSpec as GoldenSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Shelley" $ do
      GoldenSpec.spec
