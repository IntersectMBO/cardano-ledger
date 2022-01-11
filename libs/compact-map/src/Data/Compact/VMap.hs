{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Compact.VMap
  ( VG.Vector,
    VB,
    VU,
    VP,
    VS,
    VMap (..),
    empty,
    size,
    lookup,
    findWithDefault,
    member,
    notMember,
    map,
    mapMaybe,
    mapWithKey,
    filter,
    fold,
    foldl,
    foldlWithKey,
    foldMap,
    foldMapWithKey,
    fromMap,
    toMap,
    fromList,
    fromListN,
    toList,
    toAscList,
    keys,
    elems,
    fromAscList,
    fromAscListN,
    fromAscListWithKey,
    fromAscListWithKeyN,
    fromDistinctAscList,
    fromDistinctAscListN,
    internMaybe,
    null,
    splitAt,
    -- Internal types
    KV.KVMVector,
    KV.KVVector,
    KV.normalize,
    KV.normalizeM,
  )
where

import Cardano.Binary
import Control.DeepSeq
import Data.Compact.KVVector (KVVector (..))
import qualified Data.Compact.KVVector as KV
import qualified Data.Map.Strict as Map
import Data.Maybe as Maybe hiding (mapMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable as VU
import qualified GHC.Exts as Exts
import GHC.Generics (Generic)
import NoThunks.Class
import Prelude hiding (filter, foldMap, foldl, lookup, map, null, splitAt)

type VB = V.Vector

type VU = VU.Vector

type VP = VP.Vector

type VS = VS.Vector

newtype VMap kv vv k v = VMap
  { unVMap :: KVVector kv vv (k, v)
  }
  deriving (Eq, Generic, NoThunks, ToCBOR, FromCBOR, NFData, Semigroup, Monoid)

instance (Show k, Show v, VG.Vector kv k, VG.Vector vv v) => Show (VMap kv vv k v) where
  show = show . KV.toMap . unVMap

instance (VG.Vector kv k, VG.Vector vv v, Ord k) => Exts.IsList (VMap kv vv k v) where
  type Item (VMap kv vv k v) = (k, v)
  fromList = fromList
  {-# INLINE fromList #-}
  fromListN = fromListN
  {-# INLINE fromListN #-}
  toList = toAscList
  {-# INLINE toList #-}

empty :: (VG.Vector kv k, VG.Vector vv v) => VMap kv vv k v
empty = VMap VG.empty

size :: (VG.Vector kv k) => VMap kv vv k v -> Int
size = VG.length . KV.keysVector . unVMap
{-# INLINE size #-}

lookup ::
  (Ord k, VG.Vector kv k, VG.Vector vv v) => k -> VMap kv vv k v -> Maybe v
lookup k = KV.lookupKVVector k . unVMap
{-# INLINE lookup #-}

member ::
  (Ord k, VG.Vector kv k) => k -> VMap kv vv k v -> Bool
member k = KV.memberKVVector k . unVMap
{-# INLINE member #-}

notMember ::
  (Ord k, VG.Vector kv k) => k -> VMap kv vv k v -> Bool
notMember k = not . member k
{-# INLINE notMember #-}

filter ::
  (VG.Vector kv k, VG.Vector vv v) =>
  (k -> v -> Bool) ->
  VMap kv vv k v ->
  VMap kv vv k v
filter f = VMap . VG.filter (uncurry f) . unVMap
{-# INLINE filter #-}

findWithDefault ::
  (Ord k, VG.Vector kv k, VG.Vector vv v) => v -> k -> VMap kv vv k v -> v
findWithDefault a k = fromMaybe a . lookup k
{-# INLINE findWithDefault #-}

fromMap :: (VG.Vector kv k, VG.Vector vv v) => Map.Map k v -> VMap kv vv k v
fromMap = VMap . KV.fromMap
{-# INLINE fromMap #-}

toMap :: (VG.Vector kv k, VG.Vector vv v) => VMap kv vv k v -> Map.Map k v
toMap = KV.toMap . unVMap
{-# INLINE toMap #-}

toList :: (VG.Vector kv k, VG.Vector vv v) => VMap kv vv k v -> [(k, v)]
toList = VG.toList . unVMap
{-# INLINE toList #-}

toAscList :: (VG.Vector kv k, VG.Vector vv v) => VMap kv vv k v -> [(k, v)]
toAscList = VG.toList . unVMap
{-# INLINE toAscList #-}

fromList ::
  (Ord k, VG.Vector kv k, VG.Vector vv v) => [(k, v)] -> VMap kv vv k v
fromList = VMap . KV.fromList
{-# INLINE fromList #-}

fromListN ::
  (Ord k, VG.Vector kv k, VG.Vector vv v) => Int -> [(k, v)] -> VMap kv vv k v
fromListN n = VMap . KV.fromListN n
{-# INLINE fromListN #-}

fromAscList ::
  (Eq k, VG.Vector kv k, VG.Vector vv v) => [(k, v)] -> VMap kv vv k v
fromAscList = VMap . KV.fromAscList
{-# INLINE fromAscList #-}

fromAscListN ::
  (Eq k, VG.Vector kv k, VG.Vector vv v) => Int -> [(k, v)] -> VMap kv vv k v
fromAscListN n = VMap . KV.fromAscListN n
{-# INLINE fromAscListN #-}

fromAscListWithKey ::
  (Eq k, VG.Vector kv k, VG.Vector vv v) => (k -> v -> v -> v) -> [(k, v)] -> VMap kv vv k v
fromAscListWithKey f = VMap . KV.fromAscListWithKey f
{-# INLINE fromAscListWithKey #-}

fromAscListWithKeyN ::
  (Eq k, VG.Vector kv k, VG.Vector vv v) => Int -> (k -> v -> v -> v) -> [(k, v)] -> VMap kv vv k v
fromAscListWithKeyN n f = VMap . KV.fromAscListWithKeyN n f
{-# INLINE fromAscListWithKeyN #-}

fromDistinctAscList ::
  (VG.Vector kv k, VG.Vector vv v) => [(k, v)] -> VMap kv vv k v
fromDistinctAscList = VMap . KV.fromDistinctAscList
{-# INLINE fromDistinctAscList #-}

fromDistinctAscListN ::
  (VG.Vector kv k, VG.Vector vv v) => Int -> [(k, v)] -> VMap kv vv k v
fromDistinctAscListN n = VMap . KV.fromDistinctAscListN n
{-# INLINE fromDistinctAscListN #-}

map ::
  (VG.Vector vv a, VG.Vector vv b) =>
  (a -> b) ->
  VMap kv vv k a ->
  VMap kv vv k b
map f (VMap vec) = VMap (KV.mapValsKVVector f vec)
{-# INLINE map #-}

mapMaybe ::
  (VG.Vector kv k, VG.Vector vv a, VG.Vector vv b) =>
  (a -> Maybe b) ->
  VMap kv vv k a ->
  VMap kv vv k b
mapMaybe f (VMap vec) = VMap (VG.mapMaybe (\(k, x) -> (,) k <$> f x) vec)
{-# INLINE mapMaybe #-}

mapWithKey ::
  (VG.Vector kv k, VG.Vector vv a, VG.Vector vv b) =>
  (k -> a -> b) ->
  VMap kv vv k a ->
  VMap kv vv k b
mapWithKey f (VMap vec) = VMap (KV.mapWithKeyKVVector f vec)
{-# INLINE mapWithKey #-}

foldMapWithKey ::
  (VG.Vector kv k, VG.Vector vv v, Monoid m) =>
  (k -> v -> m) ->
  VMap kv vv k v ->
  m
foldMapWithKey f = VG.foldMap' (uncurry f) . unVMap
{-# INLINE foldMapWithKey #-}

foldl ::
  VG.Vector vv v =>
  (a -> v -> a) ->
  a ->
  VMap kv vv k v ->
  a
foldl f a = VG.foldl' f a . valsVector . unVMap
{-# INLINE foldl #-}

foldlWithKey ::
  (VG.Vector kv k, VG.Vector vv v) =>
  (a -> k -> v -> a) ->
  a ->
  VMap kv vv k v ->
  a
foldlWithKey f a = VG.foldl' (uncurry . f) a . unVMap
{-# INLINE foldlWithKey #-}

foldMap :: (VG.Vector vv v, Monoid m) => (v -> m) -> VMap kv vv k v -> m
foldMap f = VG.foldMap' f . valsVector . unVMap
{-# INLINE foldMap #-}

-- | Fold values monoidally
fold :: (VG.Vector vv m, Monoid m) => VMap kv vv k m -> m
fold = VG.foldMap' id . valsVector . unVMap
{-# INLINE fold #-}

keys :: VG.Vector kv k => VMap kv vv k v -> [k]
keys = VG.toList . keysVector . unVMap
{-# INLINE keys #-}

elems :: VG.Vector vv v => VMap kv vv k v -> [v]
elems = VG.toList . valsVector . unVMap
{-# INLINE elems #-}

null :: (VG.Vector vv v, VG.Vector kv k) => VMap kv vv k v -> Bool
null (VMap vec) = VG.null vec
{-# INLINE null #-}

splitAt ::
  (VG.Vector vv v, VG.Vector kv k) =>
  Int ->
  VMap kv vv k v ->
  (VMap kv vv k v, VMap kv vv k v)
splitAt i (VMap vec) = let (l, r) = VG.splitAt i vec in (VMap l, VMap r)
{-# INLINE splitAt #-}

internMaybe :: (VG.Vector kv k, Ord k) => k -> VMap kv vv k v -> Maybe k
internMaybe key = KV.internKVVectorMaybe key . unVMap
{-# INLINE internMaybe #-}
