import           Test.Tasty

import           PropertyTests (propertyTests)
import           STSTests (stsTests)
import           UnitTests (unitTests)
import           Test.Serialization(serializationTests)

tests :: TestTree
tests = testGroup "Ledger with Delegation" [unitTests, propertyTests, stsTests, serializationTests]

-- main entry point
main :: IO ()
main = defaultMain tests
