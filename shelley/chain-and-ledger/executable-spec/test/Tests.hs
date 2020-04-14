{-# Language LambdaCase #-}
import           Test.Tasty

import           Test.Shelley.Spec.Ledger.Examples.CDDL (cddlTests)
import           Test.Shelley.Spec.Ledger.Examples.Serialization (serializationTests)
import           Test.Shelley.Spec.Ledger.Examples.STSTests (stsTests)
import           Test.Shelley.Spec.Ledger.Examples.UnitTests (unitTests)
import           Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import           Test.TestScenario (TestScenario (..), mainWithTestScenario)

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  _ -> mainTests

mainTests :: TestTree
mainTests = testGroup "Ledger with Delegation"
  [ cddlTests
  , minimalPropertyTests
  , serializationTests
  , stsTests
  , unitTests
  ]

nightlyTests :: TestTree
nightlyTests = testGroup "Ledger with Delegation nightly"
  [ propertyTests
  ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
