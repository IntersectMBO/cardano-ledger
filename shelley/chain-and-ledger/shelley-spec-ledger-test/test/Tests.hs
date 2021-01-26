{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Crypto.Libsodium (sodiumInit)
import Test.Control.Iterate.SetAlgebra (setAlgTest)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Shelley.Spec.Ledger.Rewards (rewardTests)
import Test.Shelley.Spec.Ledger.STSTests (chainExamples, multisigExamples)
import Test.Shelley.Spec.Ledger.Pretty(prettyTest)
import qualified Test.Shelley.Spec.Ledger.Serialisation as Serialisation
import Test.Shelley.Spec.Ledger.UnitTests (unitTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Ledger with Delegation"
    [ minimalPropertyTests @C,
      rewardTests,
      Serialisation.tests 5,
      chainExamples,
      multisigExamples,
      unitTests,
      setAlgTest,
      prettyTest
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests @C,
      Serialisation.tests 50
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Ledger with Delegation fast"
    [ Serialisation.tests 1,
      chainExamples,
      multisigExamples,
      unitTests,
      setAlgTest,
      prettyTest
    ]

-- main entry point
main :: IO ()
main = sodiumInit >> mainWithTestScenario tests
