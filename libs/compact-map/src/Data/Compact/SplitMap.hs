{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Compact.SplitMap
  ( SplitMap,
    Split,

    -- * Map operations
    Map.empty,
    Map.singleton,
    Map.null,
    Map.size,
    Map.valid,

    -- * Element operations
    Map.insert,
    Map.delete,
    Map.lookup,
    Map.member,
    Map.notMember,
    Map.mapWithKey,

    -- * Filter
    Map.filter,
    Map.filterWithKey,

    -- * Fold
    Map.foldl',
    Map.foldrWithKey',
    Map.foldlWithKey',

    -- * Intersection
    Map.disjoint,
    Map.intersection,
    Map.intersectionWith,
    Map.intersectionWithKey,

    -- * Union
    Map.union,
    Map.unionWith,
    Map.unionWithKey,
    Map.partition,
    Map.partitionWithKey,
    Map.isSubmapOf,

    -- * Restrict/without
    extractKeysSet,
    (◁),
    restrictKeysSet,
    restrictKeysMap,
    restrictKeysSplit,
    restrictKeys,
    withoutKeysSet,
    withoutKeysMap,
    withoutKeysSplit,
    withoutKeys,

    -- * Conversion
    Map.toList,
    Map.fromList,
    Map.elems,
    Map.keys,
    toMap,
    fromMap,
    toSet,
    Map.fromSet,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (lookup, null)

type Split k = Ord k

type SplitMap k v = Map k v

-- =================================================================================
-- These 'restrictKeys' functions assume the structure holding the 'good' keys is small
-- An alternate approach is to use cross-type 'intersection' operations

-- | Partition the SplitMap according to keys in the Set. This is equivalent to:
--
-- > extractKeysSet m s === (withoutKeysSet m s, restrictKeysSet m s)
extractKeysSet :: forall k a. Split k => SplitMap k a -> Set k -> (SplitMap k a, SplitMap k a)
extractKeysSet sm = Set.foldl' f (sm, Map.empty)
  where
    f acc@(without, restrict) k =
      case Map.lookup k without of
        Nothing -> acc
        Just v ->
          let !without' = Map.delete k without
              !restrict' = Map.insert k v restrict
           in (without', restrict')

(◁) :: Split k => Set k -> SplitMap k a -> SplitMap k a
(◁) = flip restrictKeysSet

restrictKeysSet :: forall k a. Split k => SplitMap k a -> Set k -> SplitMap k a
restrictKeysSet = Map.restrictKeys

restrictKeysMap :: forall k a b. Split k => SplitMap k a -> Map k b -> SplitMap k a
restrictKeysMap m1 m2 = Map.filterWithKey (\k _ -> k `Map.member` m2) m1

restrictKeysSplit :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
restrictKeysSplit = restrictKeysMap

-- | Restrict the keys using the intersection operation
restrictKeys :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
restrictKeys = restrictKeysMap

-- =================================================================================
-- These 'withoutKeys' functions assume the structure holding the 'bad' keys is small
-- An alternate approach is to use cross-type 'intersection' operations

withoutKeysSet :: forall k a. Split k => SplitMap k a -> Set k -> SplitMap k a
withoutKeysSet = Map.withoutKeys

withoutKeysMap :: forall k a b. Split k => SplitMap k a -> Map k b -> SplitMap k a
withoutKeysMap m1 m2 = Map.filterWithKey (\k _ -> k `Map.notMember` m2) m1

withoutKeysSplit :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
withoutKeysSplit = withoutKeysMap

-- | Remove the keys using the intersection operation. if 'smap2' is small use
-- one of the other withoutKeysXX functions.
withoutKeys :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
withoutKeys = withoutKeysMap

toMap :: SplitMap k v -> Map.Map k v
toMap = id

fromMap :: Map.Map k v -> SplitMap k v
fromMap = id

toSet :: SplitMap k v -> Set.Set k
toSet = Map.keysSet
