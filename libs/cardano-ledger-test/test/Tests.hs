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
import qualified Test.Cardano.Ledger.Examples.AlonzoAPI as AlonzoAPI (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoBBODY as AlonzoBBODY (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoCollectInputs as AlonzoCollectInputs (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoInvalidTxUTXOW as AlonzoInvalidTxUTXOW (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoValidTxUTXOW as AlonzoValidTxUTXOW (tests)
import Test.Cardano.Ledger.Examples.BabbageFeatures (babbageFeatures)
import Test.Cardano.Ledger.Examples.ConwayFeatures (conwayFeatures)
import Test.Cardano.Ledger.Generic.AggPropTests (aggTests, depositTests)
import Test.Cardano.Ledger.Generic.Properties (genericProperties)
import qualified Test.Cardano.Ledger.NoThunks as NoThunks
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
  [ depositTests
  , calcPoolDistOldEqualsNew
  , Tools.tests
  , testGroup
      "STS Tests"
      [ babbageFeatures
      , conwayFeatures
      , AlonzoValidTxUTXOW.tests
      , AlonzoInvalidTxUTXOW.tests
      , AlonzoBBODY.tests
      , AlonzoAPI.tests
      , AlonzoCollectInputs.tests
      ]
  , genericProperties def
  , aggTests
  ]

nightlyTests :: [TestTree]
nightlyTests =
  defaultTests <> [NoThunks.test]
