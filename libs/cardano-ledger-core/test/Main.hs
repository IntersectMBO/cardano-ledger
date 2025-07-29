module Main where

import qualified Test.Cardano.Ledger.AddressSpec as AddressSpec
import qualified Test.Cardano.Ledger.BaseTypesSpec as BaseTypesSpec
import qualified Test.Cardano.Ledger.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.JsonSpec as JsonSpec
import qualified Test.Cardano.Ledger.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.State.StakePoolSpec as StakePoolSpec
import qualified Test.Cardano.Ledger.ToolsSpec as ToolsSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Core" $ do
      AddressSpec.spec
      BaseTypesSpec.spec
      BinarySpec.spec
      JsonSpec.spec
      PlutusSpec.spec
      StakePoolSpec.spec
      ToolsSpec.spec
