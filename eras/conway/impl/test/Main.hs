module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.BinarySpec as BinarySpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Conway" $ do
      BinarySpec.spec
