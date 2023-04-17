module Main where

import qualified Test.Cardano.Ledger.AddressSpec as AddressSpec
import qualified Test.Cardano.Ledger.BaseTypesSpec as BaseTypesSpec
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.UMapSpec as UMapSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Core" $ do
      BaseTypesSpec.spec
      AddressSpec.spec
      UMapSpec.spec
