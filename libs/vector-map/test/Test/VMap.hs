{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.VMap where

import Control.Exception
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.VMap as VMap
import Test.Common

type MapT = Map.Map Char Int

type VMapT = VMap VB VP Char Int

instance
  (Ord k, Vector kv k, Vector vv v, Arbitrary k, Arbitrary v) =>
  Arbitrary (VMap kv vv k v)
  where
  arbitrary =
    VMap.fromMap . Map.fromList <$> arbitrary

prop_Roundtrip :: (VMapT -> a) -> (a -> VMapT) -> VMapT -> Property
prop_Roundtrip to from km = from (to km) === km

prop_AsMapTo ::
  (Show a, Eq a) => (VMapT -> a) -> (MapT -> a) -> VMapT -> Property
prop_AsMapTo fromVM fromM vm = fromVM vm === fromM (toMap vm)

prop_AsMapFrom :: (a -> VMapT) -> (a -> MapT) -> a -> Property
prop_AsMapFrom mkVMap mkMap a = toMap (mkVMap a) === mkMap a

data ThunkException = ThunkException
  deriving (Eq, Show)

instance Exception ThunkException

vMapTests :: Spec
vMapTests =
  describe "VMap" $ do
    describe "roundtrip" $ do
      prop "to/fromAscDistinctList" $
        prop_Roundtrip VMap.toAscList VMap.fromDistinctAscList
      prop "to/fromAscList" $ prop_Roundtrip VMap.toAscList VMap.fromAscList
      prop "to/fromList" $ prop_Roundtrip VMap.toAscList VMap.fromList
      prop "to/fromMap" $ prop_Roundtrip VMap.toMap VMap.fromMap
    describe "strictness" $ do
      let bottom :: a
          bottom = throw ThunkException
      prop "fromList" $ \xs ys -> do
        let
          vmapKeyThunk, vmapValueThunk :: VMap VB VB String String
          vmapKeyThunk = VMap.fromList $ xs ++ [(bottom, "value")] ++ ys
          vmapValueThunk = VMap.fromList $ xs ++ [("key", bottom)] ++ ys
        evaluate vmapKeyThunk `shouldThrow` (== ThunkException)
        evaluate vmapValueThunk `shouldThrow` (== ThunkException)
      prop "map" $ \xs ys -> do
        let
          vmapValueThunk :: VMap VB VB String String
          vmapValueThunk =
            VMap.map (\v -> if v == "bottom" then bottom else v) $
              VMap.fromList $
                xs ++ [("key", "bottom")] ++ ys
        evaluate vmapValueThunk `shouldThrow` (== ThunkException)
      prop "mapWithKey" $ \xs ys -> do
        let
          vmapKeyThunk, vmapValueThunk :: VMap VB VB String String
          vmapKeyThunk =
            VMap.mapWithKey (\k v -> if k == "bottom" then bottom else v) $
              VMap.fromList $
                xs ++ [("bottom", "value")] ++ ys
          vmapValueThunk =
            VMap.mapWithKey (\_ v -> if v == "bottom" then bottom else v) $
              VMap.fromList $
                xs ++ [("key", "bottom")] ++ ys
        evaluate vmapKeyThunk `shouldThrow` (== ThunkException)
        evaluate vmapValueThunk `shouldThrow` (== ThunkException)
    describe "asMap" $ do
      prop "mapMaybeWithKey" $ \xs f ->
        prop_AsMapFrom
          (\m -> VMap.mapMaybeWithKey (applyFun2 f) (VMap.fromMap m))
          (Map.mapMaybeWithKey (applyFun2 f) :: MapT -> MapT)
          (Map.fromList xs)
      prop "fromList" $ prop_AsMapFrom VMap.fromList Map.fromList
      prop "fromAscListWithKey" $ \xs f ->
        prop_AsMapFrom
          (VMap.fromAscListWithKey (applyFun3 f))
          (Map.fromAscListWithKey (applyFun3 f))
          (List.sortOn fst xs)
      prop "fromAscListWithKeyN" $ \n xs f ->
        prop_AsMapFrom
          (VMap.fromAscListWithKeyN n (applyFun3 f))
          (Map.fromAscListWithKey (applyFun3 f) . take n)
          (List.sortOn fst xs)
      prop "unionWithKey" $ \xs1 xs2 f ->
        prop_AsMapFrom
          (\(m1, m2) -> VMap.unionWithKey (applyFun3 f) (VMap.fromMap m1) (VMap.fromMap m2))
          (uncurry (Map.unionWithKey (applyFun3 f)))
          (Map.fromList xs1, Map.fromList xs2)
      prop "mappend" $ \xs1 xs2 ->
        prop_AsMapFrom
          (\(m1, m2) -> VMap.fromMap m1 <> VMap.fromMap m2)
          (uncurry (<>))
          (Map.fromList xs1, Map.fromList xs2)
      prop "keys" $ prop_AsMapTo VMap.keys Map.keys
      prop "keysSet" $ prop_AsMapTo VMap.keysSet Map.keysSet
      prop "elems" $ prop_AsMapTo VMap.elems Map.elems
      prop "toAscList" $ prop_AsMapTo VMap.toAscList Map.toAscList
      prop "foldMapWithKey" $ \f ->
        let f' k v = applyFun2 f k v :: String
         in prop_AsMapTo (VMap.foldMapWithKey f') (Map.foldMapWithKey f')
      prop "lookup" $ \k -> prop_AsMapTo (VMap.lookup k) (Map.lookup k)
      prop "lookup (existing)" $ \k v xs ->
        let xs' = xs <> [(k, v)]
         in (VMap.lookup k (VMap.fromList xs' :: VMapT) === Just v)
              .&&. (Map.lookup k (Map.fromList xs' :: MapT) === Just v)
      prop "findWithDefault" $ \d k ->
        prop_AsMapTo (VMap.findWithDefault d k) (Map.findWithDefault d k)
      prop "findWithDefault (existing)" $ \k v xs ->
        let xs' = xs <> [(k, v)]
            unfound = error $ "the expected key " <> show k <> " was not found"
         in (VMap.findWithDefault unfound k (VMap.fromList xs' :: VMapT) === v)
              .&&. (Map.findWithDefault unfound k (Map.fromList xs' :: MapT) === v)
    testLawsGroup "classes" $
      [ eqLaws (Proxy @VMapT)
      , semigroupLaws (Proxy @VMapT)
      , monoidLaws (Proxy @VMapT)
      , isListLaws (Proxy @VMapT)
      ]
