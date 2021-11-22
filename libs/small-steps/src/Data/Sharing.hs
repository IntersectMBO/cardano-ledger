{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.Sharing
  ( emptyA,
    include,
    internI,
    internUsingMap,
    FromCBORShare (..),
    Interned (..),
    Internable (..),
    fromCBOR',
    decodeAndIntern,
    -- Extra for experts
    Arity (..),
    Nat (..),
    IList,
  )
where

import Cardano.Binary (Decoder, FromCBOR (..), decodeListLen, decodeMapSkel, dropMap)
import Control.Iterate.SetAlgebra (BiMap, biMapFromAscDistinctList)
import Control.Monad (void)
import Control.SetAlgebra (backwards)
import Data.Coders (decodeMap, decodeVMap, invalidKey)
import Data.Compact.VMap (VB, VMap)
import qualified Data.Compact.VMap as VMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Internal

-- =======================================

class Internable t k where
  makeIntern :: t -> Interned k

data Interned k = Interned
  { -- | Partially applied intern function
    internFunc :: k -> Maybe k,
    -- | Corresponds to the size of a map
    internWeight :: Int
  }

instance Ord k => Internable (Map k v) k where
  makeIntern m = Interned (go m) (Map.size m)
    where
      go Tip _ = Nothing
      go (Bin _ kx _ l r) !k =
        case compare k kx of
          LT -> go l k
          GT -> go r k
          EQ -> Just kx

instance Ord k => Internable (VMap VB v2 k v) k where
  makeIntern v = Interned (\k -> VMap.internMaybe k v) (VMap.size v)

-- | include a new Interned object in the List, the idea is to keep the List sorted by internWeight, so
--   the largest objects come first. This should decrease the number of Interned objects searched when
--   looking to intern a 'k' object.
include :: Internable t k => t -> [Interned k] -> [Interned k]
include m xs = go xs
  where
    mi = (makeIntern m)
    go [] = [mi]
    go x@(n : ns)
      | internWeight mi > internWeight n = mi : x
      | otherwise = n : (go ns)

type IList k = [Interned k]

-- type StakeID crypto = Credential 'Staking crypto
-- type PoolID crypto = KeyHash 'StakePool crypto

-- ==================================

internUsingMap :: Ord k => k -> Map k a -> k
internUsingMap = go
  where
    go !k Tip = k
    go !k (Bin _ kx _ l r) = case compare k kx of
      LT -> go k l
      GT -> go k r
      EQ -> kx

-- | Intern a key, return an Eq key, which is the same Heap object if it occurs in the IList
--   Sharing aside, (internI ilist) is the identity function.
internI :: Ord k => IList k -> k -> k
internI [] k = k
internI ((Interned findKey _weight) : ms) k =
  case findKey k of
    Just k2 -> k2
    Nothing -> internI ms k

-- ==========================================================================

-- | Provide evidence of how many objects we are interning. One can have up to 3
data Arity (n :: Nat) tuple tables where
  A1 :: Arity 'N1 x (IList x)
  A2 :: Arity 'N2 (x, y) (IList x, IList y)
  A3 :: Arity 'N3 (x, y, z) (IList x, IList y, IList z)

data Nat = N1 | N2 | N3

-- | Like (FromCBOR t), but able to perform sharing on objects of type k, because of the Arity parameter
--   k can be x or (x,y) or (x,y,z). That means we can carry around up to 3 tables to use to intern 3 differenet types.
--   An Instance can define either method 'fromShare' or 'fromSharePlus'. The latter accumulates new (Ilist tables).
class FromCBORShare t k n where
  {-# MINIMAL (fromShare | fromSharePlus) #-}
  fromShare :: Arity n k table -> table -> Decoder s t
  fromShare arity maps = fmap fst (fromSharePlus arity maps)

  fromSharePlus :: Arity n k table -> table -> Decoder s (t, table)
  fromSharePlus arity maps = (,maps) <$> fromShare arity maps

-- | Decode 't' then look it up in the intern table (IList) 'ts'
decodeAndIntern :: (Ord t, FromCBOR t) => (IList t) -> Decoder s t
decodeAndIntern ts = internI ts <$> fromCBOR

-- | Decode a Map where there are intern tables for both the domain 'k' and the range 'v'
--   Both accumulating (fromSharePlus) and normal (fromShare) methods are defined.
instance (Ord k, Ord v, FromCBOR k, FromCBOR v) => FromCBORShare (Map k v) (k, v) 'N2 where
  fromShare A2 (ks, vs) = decodeMap (decodeAndIntern ks) (decodeAndIntern vs)
  fromSharePlus A2 (ks, vs) =
    do
      mp <- decodeMap (decodeAndIntern ks) (decodeAndIntern vs)
      pure (mp, (include mp ks, vs))

-- | Decode a Map where there is an intern tables for just the domain 'k'
instance (Ord k, FromCBOR k, FromCBOR v) => FromCBORShare (Map k v) k 'N1 where
  fromShare A1 ks = decodeMap (decodeAndIntern ks) fromCBOR
  fromSharePlus A1 ks = do
    mp <- decodeMap (decodeAndIntern ks) fromCBOR
    pure (mp, include mp ks)

-- | Decode a Map where there is an intern tables for just the range 'v'
instance (Ord k, Ord v, FromCBOR k, FromCBOR v) => FromCBORShare (Map k v) v 'N1 where
  fromShare A1 vs = decodeMap fromCBOR (decodeAndIntern vs)

-- Can be used inside FromCBOR instances, when you only have a (FromShare k t) instance
-- use:  fromCBOR' A1   or fromCBOR' A2      or    fromCBOR' A3
fromCBOR' :: forall tuple t n table s. FromCBORShare t tuple n => Arity n tuple table -> Decoder s t
fromCBOR' arity = fromShare arity (emptyA arity)

emptyA :: Arity n tuple table -> table
emptyA A1 = []
emptyA A2 = ([], [])
emptyA A3 = ([], [], [])

-- ==============================================================================
-- These BiMap instances are adapted from the FromCBOR instances in Data.Coders

instance
  forall a b.
  (Ord a, Ord b, FromCBOR a, FromCBOR b) =>
  FromCBORShare (BiMap b a b) b 'N1
  where
  fromShare A1 maps =
    decodeListLen >>= \case
      1 -> decodeMapAsBimap maps
      -- Previous encoding of 'BiMap' encoded both the forward and reverse
      -- directions. In this case we skip the reverse encoding. Note that,
      -- further, the reverse encoding was from 'b' to 'a', not the current 'b'
      -- to 'Set a', and hence the dropper reflects that.
      2 -> do
        !x <- decodeMapAsBimap maps
        dropMap (void $ fromCBOR @b) (void $ fromCBOR @a)
        return x
      k -> invalidKey (fromIntegral k)
  fromSharePlus A1 maps = do
    bimap <- fromShare A1 maps
    pure (bimap, include (backwards bimap) maps)

-- | Decode a serialised CBOR Map as a Bimap
decodeMapAsBimap ::
  (FromCBOR a, FromCBOR b, Ord a, Ord b) =>
  IList b ->
  Decoder s (BiMap b a b)
decodeMapAsBimap maps = decodeMapSkel (biMapFromAscDistinctList . List.map (\(k, v) -> (k, internI maps v)))

-- ==================================================================

instance (Ord k, FromCBOR k, FromCBOR t, VMap.Vector v2 t) => FromCBORShare (VMap VB v2 k t) k 'N1 where
  fromShare A1 maps = decodeVMap (decodeAndIntern maps) fromCBOR
  fromSharePlus A1 maps = do mp <- fromShare A1 maps; pure (mp, include mp maps)

instance (Ord k, Ord t, FromCBOR k, FromCBOR t, VMap.Vector v2 t) => FromCBORShare (VMap VB v2 k t) (k, t) 'N2 where
  fromShare A2 (km, tm) = decodeVMap (decodeAndIntern km) (decodeAndIntern tm)
  fromSharePlus A2 (kms, tms) = do
    mp <- decodeVMap (decodeAndIntern kms) (decodeAndIntern tms)
    pure (mp, (include mp kms, tms))
