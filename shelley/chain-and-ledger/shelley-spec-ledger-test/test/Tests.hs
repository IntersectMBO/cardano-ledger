{-# LANGUAGE LambdaCase #-}

import Test.Control.Iterate.SetAlgebra (setAlgTest)
import Test.Shelley.Spec.Ledger.NonTraceProperties.PropertyTests (nonTracePropertyTests)
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Shelley.Spec.Ledger.Rewards (rewardTests)
import Test.Shelley.Spec.Ledger.STSTests (chainExamples)
import qualified Test.Shelley.Spec.Ledger.Serialisation as Serialisation
import Test.Shelley.Spec.Ledger.ShowTests (showTests)
import Test.Shelley.Spec.Ledger.UnitTests (unitTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  Show -> showTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Ledger with Delegation"
    [ minimalPropertyTests,
      rewardTests,
      Serialisation.tests 5,
      chainExamples,
      --multisigExamples, - TODO re-enable after the script embargo has been lifted
      unitTests,
      setAlgTest,
      showTests
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests,
      nonTracePropertyTests,
      Serialisation.tests 50,
      showTests
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Ledger with Delegation fast"
    [ Serialisation.tests 1,
      chainExamples,
      --multisigExamples, - TODO re-enable after the script embargo has been lifted
      unitTests,
      setAlgTest
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
