{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)
import qualified Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Tests as LedgerTypes
import qualified Test.Cardano.Ledger.Constrained.Conway.MiniTrace as MiniTrace
import qualified Test.Cardano.Ledger.Examples.AlonzoAPI as AlonzoAPI (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoBBODY as AlonzoBBODY (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoCollectInputs as AlonzoCollectInputs (tests)
import Test.Cardano.Ledger.Examples.BabbageFeatures (babbageFeatures)
import Test.Cardano.Ledger.Generic.AggPropTests (aggTests, depositTests)
import Test.Cardano.Ledger.Generic.GenState (defaultGenSize)
import Test.Cardano.Ledger.Generic.Properties (genericProperties)
import qualified Test.Cardano.Ledger.NoThunks as NoThunks
import qualified Test.Cardano.Ledger.STS as ConstraintSTS
import Test.Cardano.Ledger.Tickf (calcPoolDistOldEqualsNew)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  nightly <- lookupEnv "NIGHTLY"
  minitrace <- testSpec "MiniTrace" MiniTrace.spec
  case nightly of
    Nothing -> do
      defaultMain $ testGroup "cardano-core" $ minitrace : defaultTests
    Just _ -> do
      ledgerTypesSpec <- testSpec "LedgerTypes" LedgerTypes.spec
      defaultMain $ testGroup "cardano-core - nightly" $ ledgerTypesSpec : nightlyTests

defaultTests :: [TestTree]
defaultTests =
  [ depositTests
  , calcPoolDistOldEqualsNew
  , testGroup
      "STS Tests"
      [ babbageFeatures
      , AlonzoBBODY.tests
      , AlonzoAPI.tests
      , AlonzoCollectInputs.tests
      ]
  , genericProperties defaultGenSize
  , aggTests
  , ConstraintSTS.tests_STS
  ]

nightlyTests :: [TestTree]
nightlyTests =
  defaultTests <> [NoThunks.test]
