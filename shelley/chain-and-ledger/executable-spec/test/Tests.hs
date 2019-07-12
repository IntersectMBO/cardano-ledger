import Test.Tasty

import UnitTests     (unitTests)
import PropertyTests (propertyTests)
import STSTests      (stsTests)

tests :: TestTree
tests = testGroup "Ledger with Delegation" [unitTests, propertyTests, stsTests]

-- main entry point
main :: IO ()
main = defaultMain tests
