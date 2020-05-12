{-# LANGUAGE LambdaCase #-}

import Test.Shelley.Spec.Ledger.Address (addressTests)
import Test.Shelley.Spec.Ledger.CDDL (cddlTests)
import Test.Shelley.Spec.Ledger.NonTraceProperties.PropertyTests (nonTracePropertyTests)
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Shelley.Spec.Ledger.STSTests (stsTests)
import Test.Shelley.Spec.Ledger.Serialization (serializationTests)
import Test.Shelley.Spec.Ledger.UnitTests (unitTests)
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
    [ addressTests,
      cddlTests 5,
      minimalPropertyTests,
      serializationTests,
      addressTests,
      stsTests,
      unitTests
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests,
      nonTracePropertyTests,
      cddlTests 50
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Ledger with Delegation fast"
    [ addressTests,
      cddlTests 1,
      serializationTests,
      stsTests,
      unitTests
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
