module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Mary.ValueSpec as ValueSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Mary" $ do
      ValueSpec.spec
