import           Test.Tasty

import           PropertyTests (minimalPropertyTests)
import           STSTests (stsTests)
import           Test.Serialization (serializationTests)
import           UnitTests (unitTests)

tests :: TestTree
tests = testGroup "Ledger with Delegation"
  [ unitTests
  , minimalPropertyTests
  , stsTests
  , serializationTests
  ]

-- main entry point
main :: IO ()
main = defaultMain tests
