module Test.Cardano.Ledger.Plutus.ExUnits (spec) where

import Cardano.Ledger.Plutus (
  exBudgetToExUnits,
  transExUnits,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  prop "Round-trip to ExBudget" exUnitsTranslationRoundTrip

-- ExUnits should remain intact when translating to and from the plutus type
exUnitsTranslationRoundTrip :: Gen Property
exUnitsTranslationRoundTrip = do
  e <- arbitrary
  let result = exBudgetToExUnits (transExUnits e)
  pure
    $ counterexample
      ( "Before: "
          <> show e
          <> "\n After: "
          <> show result
      )
    $ result == Just e
