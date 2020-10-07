{-# LANGUAGE LambdaCase #-}

import Cardano.Crypto.Libsodium (sodiumInit)
import Data.Proxy
import Test.Control.Iterate.SetAlgebra (setAlgTest)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.MemoBytes( memoBytesTest )
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Shelley.Spec.Ledger.Rewards (rewardTests)
import Test.Shelley.Spec.Ledger.STSTests (chainExamples)
import qualified Test.Shelley.Spec.Ledger.Serialisation as Serialisation
import Test.Shelley.Spec.Ledger.UnitTests (unitTests)
import Test.Shelley.Spec.Ledger.ValProp(valTests)
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
    [ minimalPropertyTests,
      rewardTests (Proxy :: Proxy C),
      Serialisation.tests 5,
      chainExamples,
      --multisigExamples, - TODO re-enable after the script embargo has been lifted
      unitTests,
      setAlgTest,
      memoBytesTest,
      valTests
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests,
      Serialisation.tests 50
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Ledger with Delegation fast"
    [ Serialisation.tests 1,
      chainExamples,
      --multisigExamples, - TODO re-enable after the script embargo has been lifted
      unitTests,
      setAlgTest,
      memoBytesTest,
      valTests
    ]

-- main entry point
main :: IO ()
main = sodiumInit >> mainWithTestScenario tests
