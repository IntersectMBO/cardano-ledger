{-# LANGUAGE BangPatterns  #-}

module Data.CannonicalMaps
  ( CannonicalZero(..),
    cannonicalInsert,
    cannonicalMapUnion,
    cannonicalMap,
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
-- A (Map k v) is (CannonicalZero v), if it never stores a zero at type v.
-- In order to do this we need to know what 'zeroC' is, and 'joinC' has to know how to
-- joining together two maps where one of its arguments might be 'zeroC'
-- This class is strictly used in the implementation, and is not observable by the user.
-- ======================================================================================

class Eq t => CannonicalZero t where
  zeroC :: t
  joinC :: t -> t -> t

instance CannonicalZero Integer where
  zeroC = 0
  joinC = (+)

instance (Eq k, Eq v, Ord k, CannonicalZero v) => CannonicalZero (Map k v) where
  zeroC = Map.empty
  joinC = cannonicalMapUnion joinC

-- Note that the class CannonicalZero and the function cannonicalMapUnion are mutually recursive.

cannonicalMapUnion ::
  (Ord k, CannonicalZero a) =>
  (a -> a -> a) -> -- (\ left right -> ??) which side do you prefer?
  Map k a ->
  Map k a ->
  Map k a
cannonicalMapUnion _f t1 Tip = t1
cannonicalMapUnion f t1 (Bin _ k x Tip Tip) = cannonicalInsert f k x t1
cannonicalMapUnion f (Bin _ k x Tip Tip) t2 = cannonicalInsert f k x t2
cannonicalMapUnion _f Tip t2 = t2
cannonicalMapUnion f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
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
        new = (f x1 x2)
    where
      !l1l2 = cannonicalMapUnion f l1 l2
      !r1r2 = cannonicalMapUnion f r1 r2
{-# INLINEABLE cannonicalMapUnion #-}

cannonicalInsert ::
  (Ord k, CannonicalZero a) =>
  (a -> a -> a) ->
  k ->
  a ->
  Map k a ->
  Map k a
cannonicalInsert = go
  where
    go ::
      (Ord k, CannonicalZero a) =>
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
{-# INLINEABLE cannonicalInsert #-}

cannonicalMap :: (Ord k, CannonicalZero a) => (a -> a) -> Map k a -> Map k a
cannonicalMap f m = Map.foldrWithKey accum Map.empty m
  where
    accum k v ans = if new == zeroC then ans else Map.insert k new ans
      where
        new = f v
{-# INLINEABLE cannonicalMap #-}

-- Pointwise comparison assuming the map is CannonicalZero, and we assume semantically that
-- the value for keys not appearing in the map is 'zeroC'

pointWise ::
  (Ord k, CannonicalZero v) =>
  (v -> v -> Bool) ->
  Map k v ->
  Map k v ->
  Bool
pointWise _ Tip Tip = True
pointWise p Tip (m@(Bin _ _ _ _ _)) = all (zeroC `p`) m
pointWise p (m@(Bin _ _ _ _ _)) Tip = all (`p` zeroC) m
pointWise p m (Bin _ k v2 ls rs) =
  case Map.splitLookup k m of
    (lm, Just v1, rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
    _ -> False
