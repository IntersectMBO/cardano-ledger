{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babbage (BabbageEra)
import Data.Proxy (Proxy (..))
import System.Environment (lookupEnv)
import qualified Test.Cardano.Ledger.Babbage.Serialisation.Tripping as Tripping
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
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Babbage tests - nightly"
    []
