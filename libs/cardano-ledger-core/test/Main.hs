module Main where

import qualified Test.Cardano.Ledger.BaseTypesSpec as BaseTypesSpec
import qualified Test.Cardano.Ledger.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.JsonSpec as JsonSpec
import qualified Test.Cardano.Ledger.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.UMapSpec as UMapSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Core" $ do
      BaseTypesSpec.spec
      BinarySpec.spec
      JsonSpec.spec
      UMapSpec.spec
      PlutusSpec.spec
