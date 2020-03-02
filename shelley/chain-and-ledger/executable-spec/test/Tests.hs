import           Test.Tasty

--import           PropertyTests (minimalPropertyTests)
import           STSTests (stsTests)
import           Test.Serialization (serializationTests)
--import           Test.CDDL (cddlTests)
import           UnitTests (unitTests)

tests :: TestTree
tests = testGroup "Ledger with Delegation"
  --[ cddlTests -- TODO get cddl tests working in CI
  --[ minimalPropertyTests TODO stop CI timeouts
  [
    serializationTests
  , stsTests
  , unitTests
  ]

-- main entry point
main :: IO ()
main = defaultMain tests
