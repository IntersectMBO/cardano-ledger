{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Default.Class (Default (def))
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)
import qualified Test.Cardano.Ledger.Alonzo.Tools as Tools
import Test.Cardano.Ledger.Constrained.Examples (allExampleTests)
import Test.Cardano.Ledger.Constrained.Preds.Tx (predsTests)
import Test.Cardano.Ledger.Constrained.Spec (allSpecTests)
import Test.Cardano.Ledger.Constrained.Trace.Tests (conwayTrace, conwayTxwithDRepCertsTraceTests)
import qualified Test.Cardano.Ledger.Examples.AlonzoAPI as AlonzoAPI (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoBBODY as AlonzoBBODY (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoCollectInputs as AlonzoCollectInputs (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoInvalidTxUTXOW as AlonzoInvalidTxUTXOW (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoValidTxUTXOW as AlonzoValidTxUTXOW (tests)
import Test.Cardano.Ledger.Examples.BabbageFeatures (babbageFeatures)
import Test.Cardano.Ledger.Generic.AggPropTests (aggTests, depositTests)
import qualified Test.Cardano.Ledger.Generic.PrettyTest as Pretty
import Test.Cardano.Ledger.Generic.Properties (genericProperties)
import qualified Test.Cardano.Ledger.NoThunks as NoThunks
import qualified Test.Cardano.Ledger.STS as ConstraintSTS
import Test.Cardano.Ledger.Tickf (calcPoolDistOldEqualsNew)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  nightly <- lookupEnv "NIGHTLY"
  defaultMain $ case nightly of
    Nothing -> testGroup "cardano-core" defaultTests
    Just _ -> testGroup "cardano-core - nightly" nightlyTests

defaultTests :: [TestTree]
defaultTests =
  [ Pretty.prettyTest
  , allSpecTests
  , allExampleTests
  , conwayTrace
  , predsTests
  , depositTests
  , calcPoolDistOldEqualsNew
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
  , ConstraintSTS.tests_STS
  , conwayTxwithDRepCertsTraceTests
  ]

nightlyTests :: [TestTree]
nightlyTests =
  defaultTests <> [NoThunks.test]
