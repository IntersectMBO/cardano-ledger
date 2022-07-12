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
import Test.Cardano.Ledger.BaseTypes (baseTypesTests)
import Test.Cardano.Ledger.Examples.BabbageFeatures (babbageFeatures)
import Test.Cardano.Ledger.Examples.TwoPhaseValidation
  ( allTrees,
    alonzoAPITests,
    collectOrderingAlonzo,
  )
import Test.Cardano.Ledger.Generic.AggPropTests (aggTests)
import Test.Cardano.Ledger.Generic.Properties (genericProperties)
import Test.Cardano.Ledger.Model.Properties (modelUnitTests_)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)
import qualified Test.Cardano.Ledger.NoThunks as NoThunks

-- ====================================================================================

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> mainTests
  _ -> mainTests

mainTestTrees :: [TestTree]
mainTestTrees = 
    [ baseTypesTests,
      Tools.tests,
      testGroup
        "STS Tests"
        [ allTrees,
          babbageFeatures,
          alonzoAPITests,
          collectOrderingAlonzo,
          modelUnitTests_
        ],
      genericProperties def,
      aggTests
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
