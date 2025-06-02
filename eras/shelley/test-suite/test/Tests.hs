{-# LANGUAGE TypeApplications #-}

import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.Ledger.Shelley (ShelleyEra)
import Data.Proxy (Proxy (..))
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)
import Test.Cardano.Ledger.Shelley.PropertyTests (commonTests)
import qualified Test.Cardano.Ledger.Shelley.Rewards as Rewards (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.AdaPreservation as AdaPreservation
import qualified Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces as ClassifyTraces (
  onlyValidChainSignalsAreGenerated,
  relevantCasesAreCovered,
 )
import qualified Test.Cardano.Ledger.Shelley.Rules.Deposits as Deposits (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.IncrementalStake as IncrementalStake
import qualified Test.Cardano.Ledger.Shelley.RulesTests as RulesTests (
  chainExamples,
  multisigExamples,
 )
import qualified Test.Cardano.Ledger.Shelley.SafeHash as SafeHash (safeHashTest)
import qualified Test.Cardano.Ledger.Shelley.Serialisation as Serialisation
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import qualified Test.Cardano.Ledger.Shelley.UnitTests as UnitTests (unitTests)
import qualified Test.Cardano.Ledger.Shelley.WitVKeys as WitVKeys (tests)
import Test.QuickCheck (Args (maxSuccess), stdArgs)
import Test.Tasty
import Test.Tasty.QuickCheck (QuickCheckMaxRatio (..))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  sodiumInit
  nightly <- lookupEnv "NIGHTLY"
  defaultMain $ case nightly of
    Nothing -> defaultTests
    Just _ -> nightlyTests

defaultTests :: TestTree
defaultTests =
  testGroup
    "Shelley tests"
    [ Deposits.tests @ShelleyEra
    , localOption
        (QuickCheckMaxRatio 50)
        (ClassifyTraces.relevantCasesAreCovered @ShelleyEra (maxSuccess stdArgs))
    , AdaPreservation.tests @ShelleyEra (maxSuccess stdArgs)
    , ClassifyTraces.onlyValidChainSignalsAreGenerated @ShelleyEra
    , WitVKeys.tests
    , Rewards.tests
    , Serialisation.tests
    , RulesTests.chainExamples
    , RulesTests.multisigExamples
    , UnitTests.unitTests
    , SafeHash.safeHashTest
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Shelley tests - nightly"
    $ Serialisation.tests
      : IncrementalStake.incrStakeComparisonTest (Proxy :: Proxy ShelleyEra)
      : commonTests @ShelleyEra
