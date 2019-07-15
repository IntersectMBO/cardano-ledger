import           Test.Tasty

import           PropertyTests (propertyTests)
import           STSTests (stsTests)
import           UnitTests (unitTests)

tests :: TestTree
tests = testGroup "Ledger with Delegation" [unitTests, propertyTests, stsTests]

-- main entry point
main :: IO ()
main = defaultMain tests
