{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment (lookupEnv)
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
    []

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Babbage tests - nightly"
    []
