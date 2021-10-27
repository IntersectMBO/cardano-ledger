{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Compact.KVVector
  ( VG.Vector,
    VGM.MVector,
    KVVector (..),
    KVMVector,
    toMap,
    fromMap,
    fromAscList,
    fromAscListN,
    fromAscListWithKey,
    fromAscListWithKeyN,
    fromDistinctAscList,
    fromDistinctAscListN,
    fromList,
    fromListN,
    mapValsKVVector,
    mapWithKeyKVVector,
    lookupKVVector,
    lookupDefaultKVVector,
    sortAscKVMVector,
    internKVVectorMaybe,
    normalize,
    normalizeM,
  )
where

import Cardano.Binary
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Kind
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe as Maybe
import Data.Semigroup
import Data.Typeable
import Data.Vector.Algorithms.Merge
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified GHC.Exts as Exts
import GHC.Generics
import NoThunks.Class

-- | Convert a __sorted__ key/value vector into a `Map.Map`
toMap ::
  (VG.Vector kv k, VG.Vector vv v) =>
  KVVector kv vv (k, v) ->
  Map.Map k v
toMap = Map.fromDistinctAscList . VG.toList

-- | Convert a `Map.Map` into a sorted key/value vector.
fromMap ::
  (VG.Vector kv k, VG.Vector vv v) => Map.Map k v -> KVVector kv vv (k, v)
fromMap m = fromDistinctAscListN (Map.size m) $ Map.toAscList m

-- | Convert a possibly unsorted assoc list into a KVVector.
fromList ::
  (VG.Vector kv k, VG.Vector vv v, Ord k) =>
  [(k, v)] ->
  KVVector kv vv (k, v)
fromList xs = VG.create $ do
  mv <- VGM.unsafeNew (Prelude.length xs)
  forM_ (Prelude.zip [0 ..] xs) (uncurry (VGM.unsafeWrite mv))
  sortAscKVMVector mv
  removeDuplicates_ mv
{-# INLINE fromList #-}

-- | Convert a possibly unsorted assoc list into a KVVector.
fromListN ::
  (VG.Vector kv k, VG.Vector vv v, Ord k) =>
  Int ->
  [(k, v)] ->
  KVVector kv vv (k, v)
fromListN n xs = VG.create $ do
  mv <- fillWithList xs =<< VGM.unsafeNew n
  sortAscKVMVector mv
  removeDuplicates_ mv
{-# INLINE fromListN #-}

-- | Convert a sorted assoc list with distionct keys into a KVVector
fromDistinctAscList ::
  (VG.Vector kv k, VG.Vector vv v) =>
  [(k, v)] ->
  KVVector kv vv (k, v)
fromDistinctAscList xs = VG.fromListN (Prelude.length xs) xs
-- We do not use `VG.fromList`, because we need guarantees for minimal memory
-- consumption by the vector, which growing conversion algorithm implemented in
-- vector does not provide
{-# INLINE fromDistinctAscList #-}

-- | Convert a sorted assoc list with distionct keys into a KVVector. Length
-- must be supplied.
fromDistinctAscListN ::
  (VG.Vector kv k, VG.Vector vv v) =>
  Int ->
  [(k, v)] ->
  KVVector kv vv (k, v)
fromDistinctAscListN = VG.fromListN
{-# INLINE fromDistinctAscListN #-}

-- | Convert a sorted assoc list into a KVVector
fromAscList ::
  (Eq k, VG.Vector kv k, VG.Vector vv v) =>
  [(k, v)] ->
  KVVector kv vv (k, v)
fromAscList xs = fromAscListN (Prelude.length xs) xs
{-# INLINE fromAscList #-}

-- | Convert a sorted assoc list into a KVVector
fromAscListN ::
  (Eq k, VG.Vector kv k, VG.Vector vv v) =>
  Int ->
  [(k, v)] ->
  KVVector kv vv (k, v)
fromAscListN n = fromAscListWithKeyN n selectDuplicate
{-# INLINE fromAscListN #-}

-- | Fill a mutable vector with elements from the list, slicing the vector if
-- the list too short.
fillWithList ::
  (VGM.MVector v a, PrimMonad m) =>
  [a] ->
  v (PrimState m) a ->
  m (v (PrimState m) a)
fillWithList zs mv = go 0 zs
  where
    n = VGM.length mv
    go i ys
      | i == n = pure mv
      | x : xs <- ys = VGM.write mv i x >> go (i + 1) xs
      | otherwise = pure $ VGM.slice 0 i mv
{-# INLINE fillWithList #-}

fromAscListWithKey ::
  (Eq k, VG.Vector kv k, VG.Vector vv v) =>
  (k -> v -> v -> v) ->
  [(k, v)] ->
  KVVector kv vv (k, v)
fromAscListWithKey f xs = fromAscListWithKeyN (length xs) f xs
{-# INLINE fromAscListWithKey #-}

fromAscListWithKeyN ::
  (Eq k, VG.Vector kv k, VG.Vector vv v) =>
  Int ->
  (k -> v -> v -> v) ->
  [(k, v)] ->
  KVVector kv vv (k, v)
fromAscListWithKeyN n f xs
  | n <= 0 = VG.empty
  | otherwise = VG.create $ VGM.unsafeNew n >>= fillWithList xs >>= removeDuplicates f
{-# INLINE fromAscListWithKeyN #-}

mapValsKVVector ::
  (VG.Vector vv a, VG.Vector vv b) =>
  (a -> b) ->
  KVVector kv vv (k, a) ->
  KVVector kv vv (k, b)
mapValsKVVector f vec =
  KVVector {keysVector = keysVector vec, valsVector = VG.map f (valsVector vec)}
{-# INLINE mapValsKVVector #-}

mapWithKeyKVVector ::
  (VG.Vector kv k, VG.Vector vv a, VG.Vector vv b) =>
  (k -> a -> b) ->
  KVVector kv vv (k, a) ->
  KVVector kv vv (k, b)
mapWithKeyKVVector f KVVector {..} =
  KVVector
    { keysVector = keysVector,
      valsVector = VG.imap (\i -> f (keysVector VG.! i)) valsVector
    }
{-# INLINE mapWithKeyKVVector #-}

internKVVectorMaybe :: (VG.Vector kv k, Ord k) => k -> KVVector kv vv (k, v) -> Maybe k
internKVVectorMaybe key (KVVector keys _values) =
  VG.indexM keys =<< lookupIxSortedVector key keys
{-# INLINE internKVVectorMaybe #-}

-- | Look up a value by the key in a __sorted__ key/value vector. Ensure it is
-- sorted otherwise terrible things happen.
lookupKVVector ::
  (Ord k, VG.Vector kv k, VG.Vector vv v) => k -> KVVector kv vv (k, v) -> Maybe v
lookupKVVector key (KVVector keys values) =
  VG.indexM values =<< lookupIxSortedVector key keys
{-# INLINE lookupKVVector #-}

-- | Look up a value by the key in a __sorted__ key/value vector. Ensure it is
-- sorted otherwise terrible things happen.
lookupDefaultKVVector ::
  (Ord k, VG.Vector kv k, VG.Vector vv v) => v -> k -> KVVector kv vv (k, v) -> v
lookupDefaultKVVector v k = fromMaybe v . lookupKVVector k
{-# INLINE lookupDefaultKVVector #-}

-- | Perform a binary search on a sorted vector
lookupIxSortedVector ::
  (VG.Vector kv k, Ord k) => k -> kv k -> Maybe Int
lookupIxSortedVector key keys = go 0 (VG.length keys)
  where
    go !l !u = do
      guard (l < u)
      let !i = ((u - l) `div` 2) + l
      case compare key (keys VG.! i) of
        LT -> go l i
        GT -> go (i + 1) u
        EQ -> Just i
{-# INLINE lookupIxSortedVector #-}

sortAscKVMVector ::
  (VGM.MVector kmv k, VGM.MVector vmv v, Ord k, PrimMonad m) =>
  KVMVector kmv vmv (PrimState m) (k, v) ->
  m ()
sortAscKVMVector = sortBy (\(k1, _) (k2, _) -> compare k1 k2)
{-# INLINE sortAscKVMVector #-}

selectDuplicate :: k -> v -> v -> v
selectDuplicate _ v _ = v

removeDuplicates_ ::
  (VGM.MVector kmv k, VGM.MVector vmv v, Eq k, PrimMonad m) =>
  KVMVector kmv vmv (PrimState m) (k, v) ->
  m (KVMVector kmv vmv (PrimState m) (k, v))
removeDuplicates_ = removeDuplicates selectDuplicate
{-# INLINE removeDuplicates_ #-}

removeDuplicates ::
  (VGM.MVector kmv k, VGM.MVector vmv v, Eq k, PrimMonad m) =>
  (k -> v -> v -> v) ->
  KVMVector kmv vmv (PrimState m) (k, v) ->
  m (KVMVector kmv vmv (PrimState m) (k, v))
removeDuplicates f mv
  | VGM.null mv = pure mv
  | otherwise = do
    let n = VGM.length mv
        goMoved lastIx prev@(pk, pv) curIx = do
          VGM.write mv lastIx prev
          if curIx < n
            then do
              cur@(ck, cv) <- VGM.read mv curIx
              if ck == pk
                then goMoved lastIx (ck, f ck cv pv) (curIx + 1)
                else goMoved (lastIx + 1) cur (curIx + 1)
            else pure $ VGM.slice 0 (lastIx + 1) mv
        goUnmoved (pk, pv) curIx
          | curIx < n = do
            cur@(ck, cv) <- VGM.read mv curIx
            if ck == pk
              then goMoved (curIx - 1) (ck, f ck cv pv) (curIx + 1)
              else goUnmoved cur (curIx + 1)
          | otherwise = pure mv
    x0 <- VGM.read mv 0
    goUnmoved x0 1
{-# INLINE removeDuplicates #-}

normalize ::
  (VG.Vector kv k, VG.Vector vv v, Ord k) =>
  KVVector kv vv (k, v) ->
  KVVector kv vv (k, v)
normalize v = runST $ VG.thaw v >>= normalizeM >>= VG.unsafeFreeze
{-# INLINE normalize #-}

normalizeM ::
  (Ord k, PrimMonad m, VG.Vector kv k, VG.Vector vv v) =>
  KVMVector (VG.Mutable kv) (VG.Mutable vv) (PrimState m) (k, v) ->
  m (KVMVector (VG.Mutable kv) (VG.Mutable vv) (PrimState m) (k, v))
normalizeM mv = sortAscKVMVector mv >> removeDuplicates_ mv
{-# INLINE normalizeM #-}

instance (VG.Vector kv k, VG.Vector vv v, Ord k) => Semigroup (KVVector kv vv (k, v)) where
  (<>) v1 v2 = normalize (v1 VG.++ v2)
  {-# INLINE (<>) #-}
  sconcat = normalize . VG.concat . NE.toList
  {-# INLINE sconcat #-}

instance (VG.Vector kv k, VG.Vector vv v, Ord k) => Monoid (KVVector kv vv (k, v)) where
  mempty = VG.empty
  mconcat = normalize . VG.concat
  {-# INLINE mconcat #-}

type family Key e :: Type where
  Key (k, v) = k

type family Value e :: Type where
  Value (k, v) = v

data KVVector kv vv a = KVVector
  { keysVector :: !(kv (Key a)),
    valsVector :: !(vv (Value a))
  }
  deriving (Generic)

instance (VG.Vector kv k, VG.Vector vv v, Ord k) => Exts.IsList (KVVector kv vv (k, v)) where
  type Item (KVVector kv vv (k, v)) = (k, v)
  fromList = fromList
  {-# INLINE fromList #-}
  fromListN = fromListN
  {-# INLINE fromListN #-}
  toList = VG.toList
  {-# INLINE toList #-}

deriving instance (Eq (kv k), Eq (vv v)) => Eq (KVVector kv vv (k, v))

deriving instance (Show (kv k), Show (vv v)) => Show (KVVector kv vv (k, v))

data KVMVector kmv vmv s a = KVMVector
  { _keysMVector :: !(kmv s (Key a)),
    _valsMVector :: !(vmv s (Value a))
  }

type instance VG.Mutable (KVVector kv vv) = KVMVector (VG.Mutable kv) (VG.Mutable vv)

instance (NFData (kv k), (NFData (vv v))) => NFData (KVVector kv vv (k, v)) where
  rnf KVVector {..} = keysVector `deepseq` valsVector `deepseq` ()

instance (VG.Vector kv k, VG.Vector vv v) => VG.Vector (KVVector kv vv) (k, v) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (KVMVector kmv vmv) =
    KVVector <$> VG.basicUnsafeFreeze kmv <*> VG.basicUnsafeFreeze vmv

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (KVVector kv vv) = KVMVector <$> VG.basicUnsafeThaw kv <*> VG.basicUnsafeThaw vv

  {-# INLINE basicLength #-}
  -- ignore length on values, it assumed to match vector with keys
  basicLength (KVVector kv _) = VG.basicLength kv

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (KVVector kv vv) =
    KVVector (VG.basicUnsafeSlice i n kv) (VG.basicUnsafeSlice i n vv)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (KVVector kv vv) i = do
    k <- VG.basicUnsafeIndexM kv i
    v <- VG.basicUnsafeIndexM vv i
    pure (k, v)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (KVMVector kvDst vvDst) (KVVector kvSrc vvSrc) =
    VG.basicUnsafeCopy kvDst kvSrc >> VG.basicUnsafeCopy vvDst vvSrc

instance (VGM.MVector kmv k, VGM.MVector vmv v) => VGM.MVector (KVMVector kmv vmv) (k, v) where
  {-# INLINE basicLength #-}
  -- ignore length on values, it assumed to match vector with keys
  basicLength (KVMVector kmv _) = VGM.basicLength kmv

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (KVMVector kmv vmv) =
    KVMVector (VGM.basicUnsafeSlice j m kmv) (VGM.basicUnsafeSlice j m vmv)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (KVMVector kmv1 vmv1) (KVMVector kmv2 vmv2) =
    VGM.basicOverlaps kmv1 kmv2 && VGM.basicOverlaps vmv1 vmv2

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = do
    kmv1 <- VGM.basicUnsafeNew n
    vmv1 <- VGM.basicUnsafeNew n
    return (KVMVector kmv1 vmv1)

  {-# INLINE basicInitialize #-}
  basicInitialize (KVMVector kmv vmv) = VGM.basicInitialize kmv >> VGM.basicInitialize vmv

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n !(!k, !v) =
    KVMVector <$> VGM.basicUnsafeReplicate n k <*> VGM.basicUnsafeReplicate n v

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (KVMVector kmv vmv) i = do
    k <- VGM.basicUnsafeRead kmv i
    v <- VGM.basicUnsafeRead vmv i
    pure (k, v)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (KVMVector kmv vmv) i !(!k, !v) =
    VGM.basicUnsafeWrite kmv i k >> VGM.basicUnsafeWrite vmv i v

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (KVMVector kmvDst vmvDst) (KVMVector kmvSrc vmvSrc) =
    VGM.basicUnsafeCopy kmvDst kmvSrc >> VGM.basicUnsafeCopy vmvDst vmvSrc

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (KVMVector kmvDst vmvDst) (KVMVector kmvSrc vmvSrc) =
    VGM.basicUnsafeMove kmvDst kmvSrc >> VGM.basicUnsafeMove vmvDst vmvSrc

  {-# INLINE basicClear #-}
  basicClear (KVMVector kmv vmv) = VGM.basicClear kmv >> VGM.basicClear vmv

instance
  ( NoThunks (kv k),
    NoThunks (vv v),
    Typeable kv,
    Typeable vv,
    Typeable k,
    Typeable v
  ) =>
  NoThunks (KVVector kv vv (k, v))
  where
  wNoThunks c (KVVector kv vv) = (<|>) <$> wNoThunks c kv <*> wNoThunks c vv
  showTypeOf px = showsTypeRep (typeRep px) ""

instance
  (ToCBOR k, ToCBOR v, Ord k, VG.Vector kv k, VG.Vector vv v, Typeable kv, Typeable vv) =>
  ToCBOR (KVVector kv vv (k, v))
  where
  toCBOR = toCBOR . toMap

instance
  (FromCBOR k, FromCBOR v, Ord k, VG.Vector kv k, VG.Vector vv v, Typeable kv, Typeable vv) =>
  FromCBOR (KVVector kv vv (k, v))
  where
  fromCBOR = fromMap <$> fromCBOR

instance Typeable e => NoThunks (VS.Vector e) where
  wNoThunks _ !_ = pure Nothing
  showTypeOf px = showsTypeRep (typeRep px) ""

instance Typeable e => NoThunks (VP.Vector e) where
  wNoThunks _ !_ = pure Nothing
  showTypeOf px = showsTypeRep (typeRep px) ""
