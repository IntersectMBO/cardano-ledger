module Main where

import qualified Test.Cardano.Ledger.BaseTypesSpec as BaseTypesSpec
import qualified Test.Cardano.Ledger.AddressSpec as AddressSpec
import Test.Cardano.Ledger.Common

main :: IO ()
main =
  ledgerTestMain $
    describe "Core" $ do
      BaseTypesSpec.spec
      AddressSpec.spec
