{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Test.QuickCheck (Gen, Arbitrary (..))
import Test.TestScenario (TestScenario (..), mainWithTestScenario)
import qualified Cardano.Ledger.Core as Core

tests :: Gen (Core.Value C) -> TestTree
tests gv = askOption $ \case
  Nightly -> (nightlyTests gv)
  Fast -> fastTests
  _ -> (mainTests gv)

mainTests :: Gen (Core.Value C) -> TestTree
mainTests gv =
  testGroup
    "Ledger with Delegation"
    [ minimalPropertyTests gv,
      rewardTests (Proxy :: Proxy C),
      Serialisation.tests 5,
      chainExamples,
      --multisigExamples, - TODO re-enable after the script embargo has been lifted
      unitTests,
      setAlgTest,
      memoBytesTest,
      valTests
    ]

nightlyTests :: Gen (Core.Value C) -> TestTree
nightlyTests gv =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests gv,
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

-- generator to use for Value to pass to `tests`
-- NOTE ======
-- we want to be able to parametrize our value generation, so we can allow our
-- generator to vary depending on what sorts of tests
-- we are generating Values for.
-- This generator passed to `tests` is plumbed through for use in Rules and property tests
-- It will, in the future, take additional parameters, depending on what we will
-- require to be able to constrain Value generation in a useful way for those purposes
-- This generator for a Value (here instantiated to `Coin`) is ignored whenever
-- the Value ledger type is Coin
-- ======
genVl :: Gen (Core.Value C)
genVl = arbitrary

-- main entry point
main :: IO ()
main = sodiumInit >> mainWithTestScenario (tests genVl)
