{-# LANGUAGE LambdaCase #-}

import Test.Shelley.Spec.Ledger.Examples.CDDL (cddlTests)
import Test.Shelley.Spec.Ledger.Examples.STSTests (stsTests)
import Test.Shelley.Spec.Ledger.Examples.Serialization (serializationTests)
import Test.Shelley.Spec.Ledger.Examples.UnitTests (unitTests)
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Ledger with Delegation"
    [ cddlTests 5,
      minimalPropertyTests,
      serializationTests,
      stsTests,
      unitTests
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests,
      cddlTests 50
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Ledger with Delegation fast"
    [ cddlTests 1,
      serializationTests,
      stsTests,
      unitTests
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
