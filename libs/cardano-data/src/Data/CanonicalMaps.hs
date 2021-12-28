{-# LANGUAGE BangPatterns #-}

module Data.CanonicalMaps
  ( CanonicalZero (..),
    canonicalInsert,
    canonicalMapUnion,
    canonicalMap,
    pointWise,
    Map.Map,
  )
where

import Data.Map.Internal
  ( Map (..),
    balanceL,
    balanceR,
    link,
    link2,
    singleton,
    splitLookup,
  )
import qualified Data.Map.Strict as Map

-- =====================================================================================
-- Operations on Map from keys to values that are specialised to `CanonicalZero` values.
-- A (Map k v) is (CanonicalZero v), if it never stores a zero at type v.
-- In order to do this we need to know what 'zeroC' is, and 'joinC' has to know how to
-- joining together two maps where one of its arguments might be 'zeroC'
-- This class is strictly used in the implementation, and is not observable by the user.
-- ======================================================================================

class Eq t => CanonicalZero t where
  zeroC :: t
  joinC :: t -> t -> t

instance CanonicalZero Integer where
  zeroC = 0
  joinC = (+)

instance (Eq k, Eq v, Ord k, CanonicalZero v) => CanonicalZero (Map k v) where
  zeroC = Map.empty
  joinC = canonicalMapUnion joinC

-- Note that the class CanonicalZero and the function canonicalMapUnion are mutually recursive.

canonicalMapUnion ::
  (Ord k, CanonicalZero a) =>
  (a -> a -> a) -> -- (\ left right -> ??) which side do you prefer?
  Map k a ->
  Map k a ->
  Map k a
canonicalMapUnion _f t1 Tip = t1
canonicalMapUnion f t1 (Bin _ k x Tip Tip) = canonicalInsert f k x t1
canonicalMapUnion f (Bin _ k x Tip Tip) t2 = canonicalInsert f k x t2
canonicalMapUnion _f Tip t2 = t2
canonicalMapUnion f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
    Nothing ->
      if x1 == zeroC
        then link2 l1l2 r1r2
        else link k1 x1 l1l2 r1r2
    Just x2 ->
      if new == zeroC
        then link2 l1l2 r1r2
        else link k1 new l1l2 r1r2
      where
        new = f x1 x2
    where
      !l1l2 = canonicalMapUnion f l1 l2
      !r1r2 = canonicalMapUnion f r1 r2
{-# INLINEABLE canonicalMapUnion #-}

canonicalInsert ::
  (Ord k, CanonicalZero a) =>
  (a -> a -> a) ->
  k ->
  a ->
  Map k a ->
  Map k a
canonicalInsert = go
  where
    go ::
      (Ord k, CanonicalZero a) =>
      (a -> a -> a) ->
      k ->
      a ->
      Map k a ->
      Map k a
    go _ !kx x Tip = if x == zeroC then Tip else singleton kx x
    go f !kx x (Bin sy ky y l r) =
      case compare kx ky of
        LT -> balanceL ky y (go f kx x l) r
        GT -> balanceR ky y l (go f kx x r)
        EQ -> if new == zeroC then link2 l r else Bin sy kx new l r
          where
            new = f y x -- Apply to value in the tree, then the new value
{-# INLINEABLE canonicalInsert #-}

canonicalMap :: (Ord k, CanonicalZero a) => (a -> a) -> Map k a -> Map k a
canonicalMap f = Map.foldrWithKey accum Map.empty
  where
    accum k v ans = if new == zeroC then ans else Map.insert k new ans
      where
        new = f v
{-# INLINEABLE canonicalMap #-}

-- Pointwise comparison assuming the map is CanonicalZero, and we assume semantically that
-- the value for keys not appearing in the map is 'zeroC'

pointWise ::
  (Ord k, CanonicalZero v) =>
  (v -> v -> Bool) ->
  Map k v ->
  Map k v ->
  Bool
pointWise _ Tip Tip = True
pointWise p Tip m@Bin {} = all (zeroC `p`) m
pointWise p m@Bin {} Tip = all (`p` zeroC) m
pointWise p m (Bin _ k v2 ls rs) =
  case Map.splitLookup k m of
    (lm, Just v1, rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
    _ -> False
