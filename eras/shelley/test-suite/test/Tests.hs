{-# LANGUAGE TypeApplications #-}

import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyLEDGER)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)
import qualified Test.Cardano.Ledger.Shelley.Address.Bootstrap as Bootstrap (
  bootstrapHashTest,
 )
import qualified Test.Cardano.Ledger.Shelley.Pretty as Pretty (prettyTest)
import Test.Cardano.Ledger.Shelley.PropertyTests (commonTests)
import qualified Test.Cardano.Ledger.Shelley.Rewards as Rewards (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.AdaPreservation as AdaPreservation
import qualified Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces as ClassifyTraces (
  onlyValidChainSignalsAreGenerated,
  relevantCasesAreCovered,
 )
import qualified Test.Cardano.Ledger.Shelley.Rules.Deposits as Deposits (tests)
import qualified Test.Cardano.Ledger.Shelley.RulesTests as RulesTests (
  chainExamples,
  multisigExamples,
  testTickF,
 )
import qualified Test.Cardano.Ledger.Shelley.SafeHash as SafeHash (safeHashTest)
import qualified Test.Cardano.Ledger.Shelley.Serialisation as Serialisation
import qualified Test.Cardano.Ledger.Shelley.UnitTests as UnitTests (unitTests)
import qualified Test.Cardano.Ledger.Shelley.WitVKeys as WitVKeys (tests)
import Test.Cardano.Protocol.TPraos.ConcreteCryptoTypes (C)
import Test.Tasty
import Test.Tasty.QuickCheck (QuickCheckMaxRatio (..))

import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck (Args (maxSuccess), stdArgs)

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
    [ Deposits.tests @C
    , ( localOption
          (QuickCheckMaxRatio 50)
          (ClassifyTraces.relevantCasesAreCovered @C (maxSuccess stdArgs))
      )
    , AdaPreservation.tests @C @(ShelleyLEDGER C) (maxSuccess stdArgs)
    , ClassifyTraces.onlyValidChainSignalsAreGenerated @C
    , Bootstrap.bootstrapHashTest
    , WitVKeys.tests @(EraCrypto C)
    , Rewards.tests
    , Serialisation.tests 5
    , RulesTests.chainExamples
    , RulesTests.multisigExamples
    , RulesTests.testTickF
    , UnitTests.unitTests
    , Pretty.prettyTest
    , SafeHash.safeHashTest
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Shelley tests - nightly"
    $ [Serialisation.tests 50] ++ commonTests @C @(ShelleyLEDGER C)
