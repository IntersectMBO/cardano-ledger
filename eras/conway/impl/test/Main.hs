module Main where

import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.DiffSpec (conwayDiffSpecs)

main :: IO ()
main =
  ledgerTestMain $
    describe "Conway tests" $ do
      conwayDiffSpecs
