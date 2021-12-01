{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Iterate.BiMap where

import Cardano.Binary
  ( Decoder,
    DecoderError (DecoderErrorCustom),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeMapSkel,
    dropMap,
  )
import Codec.CBOR.Encoding (encodeListLen)
import Control.DeepSeq (NFData (rnf))
import Control.Monad (unless, void)
import Data.Coders (cborError, invalidKey)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NoThunks.Class (NoThunks (..))

-- =================== Basic BiMap =====================
-- For Bijections we define (BiMap v k v).  Reasons we can't use (Data.Bimap k v)
-- 1) We need to enforce that the second argument `v` is in the Ord class, when making it an Iter instance.
-- 2) The constructor for Data.BiMap is not exported, and it implements a Bijection
-- 3) Missing operation 'restrictkeys' and 'withoutkeys' make performant versions of operations  ◁ ⋪ ▷ ⋫ hard.
-- 4) Missing operation 'union', make performant versions of ∪ and ⨃ hard.
-- 5) So we roll our own which is really a (Data.Map k v) with an index that maps v to Set{k}

data BiMap v a b where MkBiMap :: (v ~ b) => !(Map.Map a b) -> !(Map.Map b (Set.Set a)) -> BiMap v a b

--  ^   the 1st and 3rd parameter must be the same:             ^   ^

biMapToMap :: BiMap v a b -> Map a b
biMapToMap (MkBiMap m _) = m

biMapFromMap ::
  (Ord k, Ord v) => Map k v -> BiMap v k v
biMapFromMap bmForward =
  MkBiMap bmForward $ foldr (uncurry $ flip addBack) Map.empty $ Map.toList bmForward

-- ============== begin necessary Cardano.Binary instances ===============
instance (Ord a, Ord b, ToCBOR a, ToCBOR b) => ToCBOR (BiMap b a b) where
  -- The `toCBOR` instance encodes only the forward map. We wrap this in a
  -- length-one list because a _previous_ encoding wrote out both maps, and we
  -- can easily use the list length token to distinguish between them.
  toCBOR (MkBiMap l _) = encodeListLen 1 <> toCBOR l

instance
  forall a b.
  (Ord a, Ord b, FromCBOR a, FromCBOR b) =>
  FromCBOR (BiMap b a b)
  where
  fromCBOR =
    decodeListLen >>= \case
      1 -> decodeMapAsBimap
      -- Previous encoding of 'BiMap' encoded both the forward and reverse
      -- directions. In this case we skip the reverse encoding. Note that,
      -- further, the reverse encoding was from 'b' to 'a', not the current 'b'
      -- to 'Set a', and hence the dropper reflects that.
      2 -> do
        !x <- decodeMapAsBimap
        dropMap (void $ fromCBOR @b) (void $ fromCBOR @a)
        return x
      k -> invalidKey (fromIntegral k)

-- | Decode a serialised CBOR Map as a Bimap
decodeMapAsBimap ::
  (FromCBOR a, FromCBOR b, Ord a, Ord b) =>
  Decoder s (BiMap b a b)
decodeMapAsBimap = do
  bimap@(MkBiMap mf mb) <- decodeMapSkel biMapFromAscDistinctList
  unless (Map.valid mf && Map.valid mb) $
    cborError $ DecoderErrorCustom "BiMap" "Expected distinct keys in ascending order"
  pure bimap

instance (NoThunks a, NoThunks b) => NoThunks (BiMap v a b) where
  showTypeOf _ = "BiMap"
  wNoThunks ctxt (MkBiMap l r) = wNoThunks ctxt (l, r)

instance NFData (BiMap v a b) where
  rnf (MkBiMap l r) = seq l (seq r ())

-- ============== end Necessary Cardano.Binary instances ===================

instance (Eq k, Eq v) => Eq (BiMap u k v) where
  (MkBiMap l _) == (MkBiMap x _) = l == x

instance (Show k, Show v) => Show (BiMap u k v) where
  show (MkBiMap l _r) = show l

addBack :: (Ord v, Ord k) => v -> k -> Map.Map v (Set.Set k) -> Map.Map v (Set.Set k)
addBack newv k m = Map.insertWith Set.union newv (Set.singleton k) m

retract :: (Ord v, Ord k) => v -> k -> Map.Map v (Set.Set k) -> Map.Map v (Set.Set k)
retract oldv k m = Map.adjust (Set.delete k) oldv m

insertBackwards :: (Ord k, Ord v) => v -> v -> k -> Map.Map v (Set.Set k) -> Map.Map v (Set.Set k)
insertBackwards oldv newv k m = addBack newv k (retract oldv k m)

insertWithBiMap :: (Ord k, Ord v) => (v -> v -> v) -> k -> v -> BiMap v k v -> BiMap v k v
insertWithBiMap comb k v (MkBiMap f b) = MkBiMap (Map.insertWith (mapflip comb) k v f) (insertBackwards oldv newv k b)
  where
    (oldv, newv) = case Map.lookup k f of Nothing -> (v, v); Just v2 -> (v2, comb v2 v)

biMapEmpty :: BiMap v k v
biMapEmpty = MkBiMap Map.empty Map.empty

-- Make a BiMap from a list of pairs.
-- The combine function comb=(\ earlier later -> later) will let elements
-- later in the list override ones earlier in the list, and comb =
-- (\ earlier later -> earlier) will keep the vaue that appears first in the list

biMapFromList :: (Ord k, Ord v) => (v -> v -> v) -> [(k, v)] -> BiMap v k v
biMapFromList comb xs = foldr addEntry biMapEmpty xs
  where
    addEntry (k, v) (MkBiMap forward backward) =
      case Map.lookup k forward of
        Nothing -> MkBiMap (Map.insertWith (mapflip comb) k v forward) (addBack v k backward)
        Just oldv -> MkBiMap (Map.insertWith (mapflip comb) k v forward) (insertBackwards oldv newv k backward)
          where
            newv = comb oldv v

-- Data.Map uses(\ new old -> ...) while our convention is (\ old new -> ...)
-- We also use this in the Basic instance for BiMap, which uses Data.Map
mapflip :: (v -> v -> v) -> (v -> v -> v)
mapflip f = (\old new -> f new old)

-- | /Warning/ - invariant that keys are distinct and in ascending order is not
-- checked. Make sure it is not violated, otherwise crazy things will happen.
biMapFromAscDistinctList ::
  (Ord k, Ord v) => [(k, v)] -> BiMap v k v
biMapFromAscDistinctList xs = MkBiMap bmForward bmBackward
  where
    bmForward = Map.fromDistinctAscList xs
    bmBackward = foldr (uncurry $ flip addBack) Map.empty xs

-- This synonym makes (BiMap v k v) appear as an ordinary Binary type contructor: (Bimap k v)
type Bimap k v = BiMap v k v

-- This operation is very fast (Log n) on BiMap, but extremely slow on other collections.
removeval :: (Ord k, Ord v) => v -> BiMap v k v -> BiMap v k v
removeval v (m@(MkBiMap m1 m2)) =
  case Map.lookup v m2 of
    Just kset -> MkBiMap (foldr (\k set -> Map.delete k set) m1 kset) (Map.delete v m2)
    Nothing -> m
