module Main where

import qualified Test.Cardano.Ledger.AddressSpec as AddressSpec
import qualified Test.Cardano.Ledger.BaseTypesSpec as BaseTypesSpec
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.CoreDiffTests (diffTests)

main :: IO ()
main =
  ledgerTestMain $
    describe "Core" $ do
      BaseTypesSpec.spec
      AddressSpec.spec
      diffTests
