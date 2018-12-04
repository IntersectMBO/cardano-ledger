import Test.Tasty

import UnitTests     (unitTests)
import PropertyTests (propertyTests)

tests :: TestTree
tests = testGroup "Ledger with Delegation" [unitTests, propertyTests]

-- main entry point
main :: IO ()
main = defaultMain tests
