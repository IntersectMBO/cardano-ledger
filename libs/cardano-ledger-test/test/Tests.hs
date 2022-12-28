{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Default.Class (Default (def))
import System.IO (hSetEncoding, stdout, utf8)
import qualified Test.Cardano.Ledger.Alonzo.Tools as Tools
import qualified Test.Cardano.Ledger.Examples.AlonzoAPI as AlonzoAPI (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoBBODY as AlonzoBBODY (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoCollectInputs as AlonzoCollectInputs (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoInvalidTxUTXOW as AlonzoInvalidTxUTXOW (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoValidTxUTXOW as AlonzoValidTxUTXOW (tests)
import Test.Cardano.Ledger.Examples.BabbageFeatures (babbageFeatures)
import Test.Cardano.Ledger.Examples.Consensus (genericConsensusTest)
import Test.Cardano.Ledger.Generic.AggPropTests (aggTests, depositTests)
import Test.Cardano.Ledger.Generic.Properties (genericProperties)
import qualified Test.Cardano.Ledger.NoThunks as NoThunks
import Test.Cardano.Ledger.Tickf (calcPoolDistOldEqualsNew)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

-- ====================================================================================

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> mainTests
  _ -> mainTests

mainTestTrees :: [TestTree]
mainTestTrees =
  [ depositTests
  , calcPoolDistOldEqualsNew
  , genericConsensusTest
  , Tools.tests
  , testGroup
      "STS Tests"
      [ babbageFeatures
      , AlonzoValidTxUTXOW.tests
      , AlonzoInvalidTxUTXOW.tests
      , AlonzoBBODY.tests
      , AlonzoAPI.tests
      , AlonzoCollectInputs.tests
      ]
  , genericProperties def
  , aggTests
  ]

nightlyTestTrees :: [TestTree]
nightlyTestTrees =
  mainTestTrees <> [NoThunks.test]

mainTests :: TestTree
mainTests = testGroup "cardano-core" mainTestTrees

nightlyTests :: TestTree
nightlyTests = testGroup "cardano-core-nightly" nightlyTestTrees

-- main entry point
main :: IO ()
main = do
  hSetEncoding stdout utf8
  mainWithTestScenario tests
