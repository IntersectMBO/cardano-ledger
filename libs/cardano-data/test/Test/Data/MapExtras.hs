{-# LANGUAGE ScopedTypeVariables #-}

module Test.Data.MapExtras where

import Data.Map.Strict (Map, restrictKeys, withoutKeys)
import Data.MapExtras
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

mapExtrasTests :: TestTree
mapExtrasTests =
  testGroup
    "ExtraMap functions"
    [ testProperty "extractKeys m s === (withoutKeys m s, restrictKeys m s)" $
        \(m :: Map Int Float) s ->
          extractKeys m s === (withoutKeys m s, restrictKeys m s)
    ]
