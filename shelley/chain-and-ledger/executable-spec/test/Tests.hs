import           Test.Tasty

import           Test.Shelley.Spec.Ledger.Examples.Serialization (serializationTests)
import           Test.Shelley.Spec.Ledger.Examples.STSTests (stsTests)
import           Test.Shelley.Spec.Ledger.Examples.UnitTests (unitTests)
import           Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests)
import           Test.Shelley.Spec.Ledger.Examples.CDDL (cddlTests)

tests :: TestTree
tests = testGroup "Ledger with Delegation"
  [ cddlTests
  , minimalPropertyTests
  , serializationTests
  , stsTests
  , unitTests
  ]

-- main entry point
main :: IO ()
main = defaultMain tests
