module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Mary.ValueSpec as ValueSpec
import qualified Test.Cardano.Ledger.Mary.BinarySpec as BinarySpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Mary" $ do
      ValueSpec.spec
      BinarySpec.spec
