module Test.Cardano.Ledger.Plutus.ExUnits (spec) where

import Cardano.Ledger.Plutus (
  exBudgetToExUnits,
  transExUnits,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  prop "Round-trip to ExBudget" exUnitsToExBudgetRoundTrip
  prop "Round-trip from ExBudget" exBudgetToExUnitsRoundTrip

-- ExUnits should remain intact when translating to and from the Plutus ExBudget type
exUnitsToExBudgetRoundTrip :: Gen Property
exUnitsToExBudgetRoundTrip = do
  e <- arbitrary
  let result = exBudgetToExUnits $ transExUnits e
  pure
    $ counterexample
      ( "Before: "
          <> show e
          <> "\n After: "
          <> show result
      )
    $ result == Just e

-- Plutus ExBudget should remain intact when translating to and from the ExUnits type
exBudgetToExUnitsRoundTrip :: Gen Property
exBudgetToExUnitsRoundTrip = do
  e <- arbitrary
  let result = transExUnits <$> exBudgetToExUnits e
  pure
    $ counterexample
      ( "Before: "
          <> show e
          <> "\n After: "
          <> show result
      )
    $ result == Just e
