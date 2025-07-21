{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain defaultTests

defaultTests :: TestTree
defaultTests =
  testGroup
    "Conway tests"
    []
