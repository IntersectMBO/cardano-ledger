{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Data.MapExtrasSpec (mapExtrasSpec) where

import Data.Map.Strict (Map, restrictKeys, withoutKeys)
import Data.MapExtras
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

mapExtrasSpec :: Spec
mapExtrasSpec =
  describe "MapExtras" $ do
    prop "extractKeys m s === (withoutKeys m s, restrictKeys m s)" $ \(m :: Map Int Float) s ->
      extractKeys m s === (withoutKeys m s, restrictKeys m s)
