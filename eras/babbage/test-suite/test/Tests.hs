{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (lookupEnv)
import qualified Test.Cardano.Ledger.Babbage.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Babbage.Serialisation.Tripping as Tripping
import Test.Cardano.Ledger.Babbage.TxInfo (txInfoTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  nightly <- lookupEnv "NIGHTLY"
  defaultMain $ case nightly of
    Nothing -> defaultTests
    Just _ -> nightlyTests

defaultTests :: TestTree
defaultTests =
  testGroup
    "Babbage tests"
    [ Tripping.tests
    , txInfoTests
    , CDDL.tests 5
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Babbage tests - nightly"
    [ CDDL.tests 50
    ]
