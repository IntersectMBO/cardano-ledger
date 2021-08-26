{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- HeapWords for Array and PrimArray
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Explores representing maps more compactly using this strategy
--   Split the map into three parts
--   1) A large stable part, that is not expected to change, stored as a ByteString
--   2) A (we hope much smaller) subset of the map, stored as a Map.Map
--   3) A set of changes, stored as a (Delta k v) (newtype around (Map.Map k (Message v))).
module Data.CompactMap where

import Cardano.Binary
  ( FromCBOR,
    ToCBOR (..),
    serialize',
    unsafeDeserialize',
  )
import Cardano.Prelude (HeapWords (..))
import qualified Data.Array as A
import Data.ByteString (ByteString)
import Data.Foldable (foldr')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Messages
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayFromList, primArrayToList, sizeofPrimArray)
import Data.Primitive.Types (Prim (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts (IsList (..))

-- =============================================================

data CompactMap k v = CompactMap
  { stable :: ByteString,
    subset :: Map k v,
    delta :: Delta k v
  }

initMap :: (Ord k, ToCBOR k, ToCBOR v) => Map k v -> Set k -> CompactMap k v
initMap m keys = CompactMap bytes keysmap (Delta Map.empty)
  where
    keysmap = Set.foldl' accum Map.empty keys
    accum ans key = case Map.lookup key m of
      Nothing -> ans
      Just v -> Map.insert key v ans
    bytes = serialize' (Map.withoutKeys m keys)

insert :: (Ord k, Exp v) => k -> v -> CompactMap k v -> CompactMap k v
insert k v (CompactMap s active1 m1) = CompactMap s active2 (m2 <> m1)
  where
    m2 = Delta (Map.singleton k (Edit v))
    active2 = applyMessages active1 m2

delete :: (Ord k, Exp v) => k -> CompactMap k v -> CompactMap k v
delete k (CompactMap s active1 m1) = CompactMap s active2 (m2 <> m1)
  where
    m2 = Delta (Map.singleton k Delete)
    active2 = applyMessages active1 m2

lookup :: Ord k => k -> CompactMap k v -> Maybe v
lookup key (CompactMap _ active _) = Map.lookup key active

instance (HeapWords k, HeapWords v) => HeapWords (CompactMap k v) where
  heapWords (CompactMap s a m) = heapWords s + heapWords a + heapWords m + 4

-- ============================================================================================
-- Arrays that support Binary search

class Indexable t a where
  index :: t a -> Int -> a
  isize :: t a -> Int
  fromlist :: [a] -> t a
  tolist :: t a -> [a]

-- | Find the index of 'k'. Use 'lo' and 'hi' to narrow the scope where 'k' may occur
--   This is possible because we assume 'arr' is maintained in ascending order of keys.
binsearch :: (Ord k, Indexable arr k) => Int -> Int -> k -> arr k -> Maybe Int
binsearch lo hi _k _v | lo > hi = Nothing
binsearch lo hi k v | lo == hi = if index v lo == k then Just lo else Nothing
binsearch lo _hi k v | index v lo == k = Just lo
binsearch _lo hi k v | index v hi == k = Just hi
binsearch lo hi _k _v | lo + 1 == hi = Nothing
binsearch lo hi k v = (if index v mid > k then binsearch lo mid k v else binsearch mid hi k v)
  where
    mid = lo + (div (hi - lo) 2)

-- | Find the index of 'target'
search :: (Ord k, Indexable arr k) => k -> arr k -> Maybe Int
search key v = binsearch 0 (isize v - 1) key v

-- | Find the index and the value at the least upper bound of 'target'
alub :: (Ord t1, Indexable t2 t1) => (Int, Int) -> t2 t1 -> t1 -> Maybe (Int, t1)
alub (lo, hi) arr target
  | lo > hi = Nothing
  | target <= index arr lo = Just (lo, index arr lo)
  | lo == hi = Nothing
  | lo + 1 == hi && index arr lo < target && target <= index arr hi = Just (hi, index arr hi)
  | True = if target <= index arr mid then (alub (lo, mid) arr target) else (alub (mid, hi) arr target)
  where
    mid = lo + (div (hi - lo) 2)

instance Prim a => Indexable PrimArray a where
  index = indexPrimArray
  isize = sizeofPrimArray
  fromlist = primArrayFromList
  tolist = toList

instance Indexable (A.Array Int) a where
  index = (A.!)
  isize arr = (hi - lo) + 1 where (lo, hi) = A.bounds arr
  fromlist xs = (A.listArray (0, length xs -1) xs)
  tolist arr = foldr (:) [] arr

instance (HeapWords v) => HeapWords (A.Array Int v) where
  heapWords arr = foldl accum (3 + n) arr
    where
      accum ans v = ans + heapWords v
      n = isize arr

instance (Prim a, HeapWords a) => HeapWords (PrimArray a) where
  heapWords arr = 2 + (sizeofPrimArray arr * heapWords (index arr 0))

-- ===================================================================================

-- | A parallel array impementation of a key-value store. Recall a parallel array implementation has 2 arrays.
--   one array of sorted keys, and another of the same size with corresponding values at the same index as its key.
--   The important distinction here is that the second array serializes 'n' consecutive 'v' values into one block.
--   This shrinks the heapWords of the parallel array by a factor of 'n' and gets better compaction
--   by serializing 'n' 'v' values into one ByteString, so the overheard is greatly reduced.
--   Recall that in-memory, the bytestring takes up more space than just the length of the bytestring
--   there's overhead for a pointer and tracking offsets and length. If 'v' is a small
--   then the relative amount of overhead is considerable. We share this overhead across 'n' values.
data Par2 k v where
  Par2 ::
    (Prim k, ToCBOR v) =>
    Int -> -- Compaction factor 'n'
    (PrimArray k) -> -- Sorted Unboxed array of keys
    (A.Array Int ByteString) -> -- Array of ('n' values) serialized into one ByteString
    (Map.Map k (Message v)) -> -- Difference set
    Par2 k v

-- ==================================
-- Some operations on Par2

-- | Look for a key inside a (Par2 k v), Time proportional to (log m * n) where
--   'm' is the number of keys, and 'n' is the compacting factor.
look2 :: forall k v. (Ord k, Prim k, Exp v, FromCBOR v) => k -> Par2 k v -> Maybe v
look2 k (Par2 n keys subarray diffs) =
  let findAndApply i f =
        let vals :: [v]
            vals = unsafeDeserialize' (index subarray (i `div` n))
         in Just $ applyExp f (vals !! (i `mod` n))
   in case (Map.lookup k diffs, search k keys) of
        (Nothing, Nothing) -> Nothing
        (Just (Edit v), _) -> Just v
        (Just Delete, _) -> Nothing
        (Just (Upsert _), Nothing) -> Nothing
        (Just (Upsert f), Just i) -> findAndApply i f
        (Nothing, Just i) -> findAndApply i Identity

insert2 :: Ord k => k -> v -> Par2 k v -> Par2 k v
insert2 k v (Par2 n keys vs diff) = Par2 n keys vs (Map.insert k (Edit v) diff)

delete2 :: Ord k => k -> Par2 k v -> Par2 k v
delete2 k (Par2 n keys vs diff) = Par2 n keys vs (Map.insert k Delete diff)

getValues :: forall k v. (FromCBOR v) => (Par2 k v) -> [v]
getValues (Par2 _n _keys vs _diff) = foldr' accum [] vs
  where
    accum bytes ans = (unsafeDeserialize' bytes :: [v]) ++ ans

-- | Get an ascending List of pairs from the Par2, apply the diff set as it is computed
getPairs :: forall k v. (Ord k, Exp v, FromCBOR v) => (Par2 k v) -> [(k, v)]
getPairs (Par2 _n keys vs diff) = foldr' accum2 [] (zip (primArrayToList keys) (foldr' accum [] vs))
  where
    accum bytes ans = (unsafeDeserialize' bytes :: [v]) ++ ans
    accum2 (k, v) ans = case Map.lookup k diff of
      Nothing -> (k, v) : ans
      Just (Edit u) -> (k, u) : ans
      Just Delete -> ans
      Just (Upsert f) -> (k, applyExp f v) : ans

-- ==========================================================================
-- Constructing a Par2 from a Map

-- | Convert a (Map.Map k v) into a (Par2 k v), with compaction factor 'n'
toPar2 :: (Prim k, ToCBOR v) => Int -> Map.Map k v -> Par2 k v
toPar2 n m = Par2 n keys (serializedSublistArray n pairs) (Map.empty)
  where
    pairs = Map.toAscList m
    keys = primArrayFromList (map fst pairs)

groupN :: Int -> [a] -> [[a]]
groupN _n [] = []
groupN n xs = take n xs : groupN n (drop n xs)

serializedSublistArray :: (ToCBOR b) => Int -> [(a, b)] -> A.Array Int ByteString
serializedSublistArray n pairs =
  A.array (0, size -1) ([(i, serialize' (map snd sublist)) | (i, sublist) <- zip [0 .. size -1] (groupN n pairs)])
  where
    numpairs = length pairs
    size = if numpairs `mod` n == 0 then numpairs `div` n else (numpairs `div` n) + 1

instance (HeapWords v, HeapWords k) => HeapWords (Par2 k v) where
  heapWords (Par2 _ ks vs diff) = 5 + heapWords ks + heapWords vs + heapWords diff

instance (Show k, Show v, FromCBOR v, Prim k) => Show (Par2 k v) where
  show (Par2 n ks vs diff) = "Par2 " ++ show n ++ "\n  " ++ show ks ++ "\n  " ++ show us ++ "\n  " ++ show diff
    where
      us :: [[v]]
      us = map unsafeDeserialize' (foldr (:) [] vs)
