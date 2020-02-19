import           Test.Tasty

import           PropertyTests (minimalPropertyTests)
import           STSTests (stsTests)
import           Test.Serialization (serializationTests)
import           Test.CDDL (cddlTests)
import           UnitTests (unitTests)

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
