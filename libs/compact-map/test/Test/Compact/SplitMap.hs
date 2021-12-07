{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Compact.SplitMap where

import Data.Compact.KeyMap (Key (..))
import Data.Compact.SplitMap
import qualified Data.Compact.SplitMap as SplitMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Word (Word64)
import Test.Compact.KeyMap
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude hiding (lookup)

-- ==============================================================
-- All the features of TxIn, but we don't need to import it

newtype MockTxIn = MockTxIn (Int, Key)
  deriving (Show, Eq, Ord)

instance Split MockTxIn where
  splitKey (MockTxIn pair) = pair
  joinKey n k = MockTxIn (n, k)

instance Arbitrary MockTxIn where
  arbitrary = joinKey <$> chooseInt (0, 25) <*> arbitrary

-- =================================================================
-- A simple type with a Split instance

data SS = SS Int Word64
  deriving (Eq, Show)

instance Ord SS where
  compare (SS i n) (SS j m) =
    case compare i j of
      EQ -> compare n m
      other -> other

instance Split SS where
  splitKey (SS n m) = (n, Key m 0 0 0)
  joinKey n (Key m _ _ _) = SS n m

instance Arbitrary SS where
  arbitrary = joinKey <$> chooseInt (0, 25) <*> (Key <$> arbitrary <*> pure 0 <*> pure 0 <*> pure 0)

-- ===============================================

instance (Split k, Arbitrary k, Arbitrary a) => Arbitrary (SplitMap k a) where
  arbitrary = do
    let go i m
          | i > 0 = do
            key <- arbitrary
            val <- arbitrary
            go (i - 1) $! insert key val m
          | otherwise = pure m
    NonNegative n <- arbitrary
    go (n :: Int) empty

-- ===============================================
-- Property tests between SplitMaps

ascFoldDescFoldx :: SplitMap k Int -> Property
ascFoldDescFoldx x = foldrWithKey' (\_key ans v -> ans + v) 0 x === foldlWithKey' (\ans _key v -> v + ans) 0 x

allKeyx :: (k -> Bool) -> SplitMap k t -> Bool
allKeyx p = foldlWithKey' (\ans key _v -> p key && ans) True

allValx :: (t -> Bool) -> SplitMap k t -> Bool
allValx p = foldlWithKey' (\ans _key v -> p v && ans) True

minKeyx :: (Split k, Show k, Show a, Ord k) => SplitMap k a -> Property
minKeyx x =
  case lookupMin x of
    Just (k, _v) -> counterexample ("min=" ++ show k ++ "map=\n" ++ show x) (allKeyx (\x1 -> x1 >= k) x)
    Nothing -> True === True

maxKeyx :: (Split k, Show k, Show a, Ord k) => SplitMap k a -> Property
maxKeyx x = case lookupMax x of
  Nothing -> True === True
  Just (k, _v) -> counterexample ("max=" ++ show k ++ "map=\n" ++ show x) (allKeyx (\x1 -> x1 <= k) x === True)

mapWorksx :: SplitMap k Int -> Property
mapWorksx x = allValx (== (99 :: Int)) (mapWithKey (\_key _x -> 99) x) === True

foldintersectS :: forall k. SplitMap k Int -> SplitMap k Int -> Property
foldintersectS x y = foldOverIntersection (\ans _key u _v -> ans + u) 0 x y === foldlWithKey' (\ans _key u -> ans + u) 0 (intersection x y)

withoutRestrictS :: (Eq k, Split k, Show k) => SplitMap k Int -> Set k -> Property
withoutRestrictS m domset = union (withoutKeysSet m domset) (restrictKeysSet m domset) === m

withoutRestrictM :: (Eq k, Split k, Show k) => SplitMap k Int -> Map k Char -> Property
withoutRestrictM m domset = union (withoutKeysMap m domset) (restrictKeysMap m domset) === m

withoutRestrictSp :: (Eq k, Split k, Show k) => SplitMap k Int -> SplitMap k Char -> Property
withoutRestrictSp m domset = union (withoutKeysSplit m domset) (restrictKeysSplit m domset) === m

withoutRestrictSp2 :: (Eq k, Split k, Show k) => SplitMap k Int -> SplitMap k Char -> Property
withoutRestrictSp2 m domset = union (withoutKeys m domset) (restrictKeys m domset) === m

splitwholeS :: (Eq k, Show k, Split k) => k -> SplitMap k Int -> Property
splitwholeS k m =
  case splitLookup k m of
    (m1, Nothing, m2) -> m === union m1 m2
    (m1, Just v, m2) -> m === (insert k v (union m1 m2))

testWhen :: Test k => SplitMap k Int -> SplitMap k Int -> Bool
testWhen xs ys = intersectionWhen p xs ys == filterWithKey q (intersectionWith r xs ys)
  where
    q _k v = even v
    r _u v = v
    p k u v = if (q k v) then Just (r u v) else Nothing

-- ===============================================

testp :: IO (SplitMap SS Int)
testp = do
  ss <- generate (vectorOf 100 (arbitrary :: Gen SS))
  let pairs = zip ss [0 .. 30]
  pure (fromList pairs)

splitIsMapTests :: TestTree
splitIsMapTests =
  testGroup
    "SpitMap operations have map-like properties"
    [ testProperty "to/fromList SS" (roundtripFromList @SS @Int toList fromList),
      testProperty "to/fromList MockTxIn" (roundtripFromList @MockTxIn @Int toList fromList),
      testProperty "SplitMap SS insert-delete" $ insertDelete @SS @Int insert delete,
      testProperty "SplitMap MockTxIn insert-delete" $ insertDelete @MockTxIn @Int insert delete,
      testProperty "SplitMap SS foldl" $ foldlkey @SS @Int toList foldlWithKey',
      testProperty "SplitMap MockTxIn foldl" $ foldlkey @MockTxIn @Int toList foldlWithKey',
      testProperty "union-intersect-property" $ setprop @SS @Int unionWithKey intersection,
      testPropertyN 1000 "union-intersect-property" $ setprop @MockTxIn @Int unionWithKey intersection,
      testProperty "lookup-insert SS" $ lookupinsert @SS @Int SplitMap.lookup insert,
      testProperty "lookup-insert MockTxIn" $ lookupinsert @MockTxIn @Int SplitMap.lookup insert,
      testProperty "lookup-delete SS" $ lookupdelete @SS @Int SplitMap.lookup delete,
      testProperty "lookup-delete MockTxIn" $ lookupdelete @MockTxIn @Int SplitMap.lookup delete,
      testPropertyN 1000 "union is associative" $ assoc @MockTxIn @Int union,
      testPropertyN 1000 "unionWith is commutative" $ commutes @MockTxIn @Int (unionWith (+)),
      -- (unionwith f) is commutative if 'f' is commutative
      testPropertyN 1000 "intersect is associative" $ assoc @MockTxIn @Int intersection,
      testPropertyN 1000 "intersectWith is commutative" $ commutes @SS @Int (intersectionWith (+)),
      -- (unionwith f) is commutative if 'f' is commutative
      testProperty "ascending fold == descending fold with commutative operator" (ascFoldDescFoldx @SS),
      testProperty "lookupMin finds the smallest key" (minKeyx @SS @Int),
      testProperty "lookupMax finds the largest key" (maxKeyx @SS @Int),
      testProperty "(mapWithKey f) applies 'f' to every value" (mapWorksx @MockTxIn),
      testProperty "foldOverIntersection folds over the intersection" (foldintersectS @MockTxIn),
      testProperty "restrictKeysSet and withoutKeysSet partition a KeyMap" (withoutRestrictS @MockTxIn),
      testProperty "restrictKeysMap and withoutKeysMap partition a KeyMap" (withoutRestrictM @MockTxIn),
      testProperty "restrictKeysSplit and withoutKeysSplit partition a KeyMap" (withoutRestrictSp @MockTxIn),
      testProperty "restrictKeys and withoutKeys partition a KeyMap" (withoutRestrictSp2 @SS),
      testPropertyN 500 "splitLookup pieces add to the whole" (splitwholeS @SS),
      testProperty "intersectWhen is filter after intersection" (testWhen @SS)
    ]

-- =========================================================
-- SplitMap functions behave the same as Data.Map

infix 4 %==%

(%==%) :: (Split k, Eq k, Eq v, Show k, Show v) => SplitMap k v -> Map.Map k v -> Property
(%==%) x y =
  counterexample
    ("\nkeymap\n" ++ show x ++ "\ndatamap\n" ++ show y)
    (toList x === Map.toList y)

type Test k = (Split k, Eq k, Show k, Ord k)

insertSPLITDATA :: forall k v. (Test k, Eq v, Show v) => k -> v -> [(k, v)] -> Property
insertSPLITDATA k v m = insert k v (fromList m) %==% Map.insert k v (Map.fromList m)

deleteSPLITDATA :: forall k v. (Test k, Eq v, Show v) => k -> [(k, v)] -> Property
deleteSPLITDATA k m = delete k (fromList m) %==% Map.delete k (Map.fromList m)

unionSPLITDATA :: forall k v. (Test k, Eq v, Show v) => [(k, v)] -> [(k, v)] -> Property
unionSPLITDATA n m = union (fromList n) (fromList m) %==% Map.union (Map.fromList n) (Map.fromList m)

intersectSPLITDATA :: forall k v. (Test k, Eq v, Show v) => [(k, v)] -> [(k, v)] -> Property
intersectSPLITDATA n m = intersection (fromList n) (fromList m) %==% Map.intersection (Map.fromList n) (Map.fromList m)

lookupSPLITDATA :: forall k v. (Test k, Eq v, Show v) => k -> [(k, v)] -> Property
lookupSPLITDATA k m = lookup k (fromList m) === Map.lookup k (Map.fromList m)

restrictSPLITDATA :: (Test k, Show a, Eq a) => [(k, a)] -> Set k -> Property
restrictSPLITDATA m s = restrictKeysSet (fromList m) s %==% Map.restrictKeys (Map.fromList m) s

withoutSPLITDATA :: (Test k, Show a, Eq a) => [(k, a)] -> Set k -> Property
withoutSPLITDATA m s = withoutKeysSet (fromList m) s %==% Map.withoutKeys (Map.fromList m) s

minSPLITDATA :: (Test k, Eq a, Show a) => [(k, a)] -> Property
minSPLITDATA m = lookupMin (fromList m) === Map.lookupMin (Map.fromList m)

maxSPLITDATA :: (Test k, Eq a, Show a) => [(k, a)] -> Property
maxSPLITDATA m = lookupMax (fromList m) === Map.lookupMax (Map.fromList m)

splitSPLITDATA :: (Test k, Eq a, Show a) => k -> [(k, a)] -> Property
splitSPLITDATA k m = case Map.splitLookup k (Map.fromList m) of
  (a, b, c) -> (fromList (Map.toList a), b, fromList (Map.toList c)) === splitLookup k (fromList m)

minViewSPLITDATA :: Test k => [(k, Int)] -> Property
minViewSPLITDATA m = case Map.minViewWithKey (Map.fromList m) of
  Nothing -> minViewWithKey (fromList m) === Nothing
  Just (b, c) -> Just (b, fromList (Map.toList c)) === minViewWithKey (fromList m)

maxViewSPLITDATA :: Test k => [(k, Int)] -> Property
maxViewSPLITDATA m = case Map.maxViewWithKey (Map.fromList m) of
  Nothing -> maxViewWithKey (fromList m) === Nothing
  Just (b, c) -> Just (b, fromList (Map.toList c)) === maxViewWithKey (fromList m)

splitMapEquivDataMap :: TestTree
splitMapEquivDataMap =
  testGroup
    "SplitMap behaves like Data.Map"
    [ testPropertyN 500 "insert" (insertSPLITDATA @SS @Int),
      testPropertyN 500 "delete" (deleteSPLITDATA @SS @Int),
      testPropertyN 500 "union" (unionSPLITDATA @SS @Int),
      testPropertyN 500 "intersection" (intersectSPLITDATA @SS @Int),
      testPropertyN 500 "lookup" (lookupSPLITDATA @SS @Int),
      testPropertyN 500 "withoutKeys" (withoutSPLITDATA @SS @Int),
      testPropertyN 500 "restrictKeys" (restrictSPLITDATA @SS @Int),
      testPropertyN 500 "lookupMin" (minSPLITDATA @SS @Int),
      testPropertyN 500 "lookupMax" (maxSPLITDATA @SS @Int),
      testPropertyN 500 "splitLookup" (splitSPLITDATA @SS @Int),
      testPropertyN 500 "minViewWithKey" (minViewSPLITDATA @SS),
      testPropertyN 500 "maxViewWithKey" (maxViewSPLITDATA @SS)
    ]

splitMapTests :: TestTree
splitMapTests = testGroup "SplitMap tests" [splitIsMapTests, splitMapEquivDataMap]
