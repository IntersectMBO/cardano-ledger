{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Data.MapExtrasSpec (mapExtrasSpec) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras
import Test.Cardano.Data
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

mapExtrasSpec :: Spec
mapExtrasSpec =
  describe "MapExtras" $ do
    prop "extract k m === (lookup k m, delete k m)" $ \(m :: Map Int Float) k -> do
      let (v, ma) = extract k m
      expectValidMap ma
      ma `shouldBe` Map.delete k m
      v `shouldBe` Map.lookup k m
    prop "extractKeys m s === (withoutKeys m s, restrictKeys m s)" $ \(m :: Map Int Float) s -> do
      let (wk, rk) = extractKeys m s
      expectValidMap wk
      expectValidMap rk
      wk `shouldBe` Map.withoutKeys m s
      rk `shouldBe` Map.restrictKeys m s
    prop "noKeys == withoutKeys" $ \(m1 :: Map Int Float) (m2 :: Map Int Char) -> do
      let nk = noKeys m1 m2
      expectValidMap nk
      nk `shouldBe` Map.withoutKeys m1 (Map.keysSet m2)
    prop "intersectDomP" $ \fun (m1 :: Map Int Char) (m2 :: Map Int Word) -> do
      let f = applyFun2 fun :: Int -> Word -> Bool
          ma = intersectDomP f m1 m2
      expectValidMap ma
      ma `shouldBe` Map.filterWithKey f (Map.intersection m2 m1)
    prop "intersectDomPLeft" $ \fun (m1 :: Map Int Char) (m2 :: Map Int Word) -> do
      let f = applyFun2 fun :: Int -> Word -> Bool
          ma = intersectDomPLeft f m1 m2
      expectValidMap ma
      ma `shouldBe` Map.mapMaybeWithKey (\k v -> v <$ (guard . f k =<< Map.lookup k m2)) m1
