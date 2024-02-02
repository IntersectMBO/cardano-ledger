{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Sometimes we need to write our own version of functions over `Map.Map` that
-- do not appear in the "containers" library. This module is for such functions.
--
-- For example:
--
-- 1. Version of `Map.withoutKeys` where both arguments are `Map.Map`
-- 2. Comparing that two maps have exactly the same set of keys
-- 3. The intersection of two maps guarded by a predicate.
--
--    > ((dom stkcred) ◁ deleg) ▷ (dom stpool)) ==>
--    > intersectDomP (\ k v -> Map.member v stpool) stkcred deleg
module Data.MapExtras (
  StrictTriple (..),
  extract,
  noKeys,
  keysEqual,
  splitMemberMap,
  splitMemberSet,
  intersectDomP,
  intersectDomPLeft,
  intersectMapSetFold,
  disjointMapSetFold,
  extractKeys,
  extractKeysSmallSet,
  fromKeys,
  fromElems,
)
where

import Data.Foldable (toList)
import Data.Map.Internal (Map (..), balanceL, balanceR, glue, link, link2)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Internal as Set
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#, (==#))

data StrictTriple a b c = StrictTriple !a !b !c
  deriving (Show, Eq)

noKeys :: Ord k => Map k a -> Map k b -> Map k a
noKeys Tip _ = Tip
noKeys m Tip = m
noKeys m (Bin _ k _ ls rs) = case Map.split k m of
  (lm, rm) -> link2 lm' rm' -- We know `k` is not in either `lm` or `rm`
    where
      !lm' = noKeys lm ls
      !rm' = noKeys rm rs
{-# INLINEABLE noKeys #-}

-- | Checks if two pointers are equal. Yes means yes;
-- no means maybe. The values should be forced to at least
-- WHNF before comparison to get moderately reliable results.
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}

keysEqual :: Ord k => Map k v1 -> Map k v2 -> Bool
keysEqual Tip Tip = True
keysEqual Tip (Bin _ _ _ _ _) = False
keysEqual (Bin _ _ _ _ _) Tip = False
keysEqual m (Bin _ k _ ls rs) =
  case splitMemberMap k m of
    StrictTriple lm True rm -> keysEqual ls lm && keysEqual rs rm
    _ -> False
{-# INLINEABLE keysEqual #-}

-- | A variant of 'splitLookup' that indicates only whether the
-- key was present, rather than producing its value. This is used to
-- implement 'keysEqual' to avoid allocating unnecessary 'Just'
-- constructors.
--
-- /Note/ - this is a copy pasted internal function from "containers" package
-- adjusted to return `StrictTriple`
splitMemberMap :: Ord k => k -> Map k a -> StrictTriple (Map k a) Bool (Map k a)
splitMemberMap = go
  where
    go :: Ord k => k -> Map k a -> StrictTriple (Map k a) Bool (Map k a)
    go !k t =
      case t of
        Tip -> StrictTriple Tip False Tip
        Bin _ kx x l r -> case compare k kx of
          LT ->
            let !(StrictTriple lt z gt) = go k l
                !gt' = link kx x gt r
             in StrictTriple lt z gt'
          GT ->
            let !(StrictTriple lt z gt) = go k r
                !lt' = link kx x l lt
             in StrictTriple lt' z gt
          EQ -> StrictTriple l True r
{-# INLINEABLE splitMemberMap #-}

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
--
-- This is a modified version of `Set.splitMember`, where `StrictTriple` is used
-- instead of a lazy one for minor performance gain.
splitMemberSet :: Ord a => a -> Set a -> StrictTriple (Set a) Bool (Set a)
splitMemberSet _ Set.Tip = StrictTriple Set.Tip False Set.Tip
splitMemberSet x (Set.Bin _ y l r) =
  case compare x y of
    LT ->
      let !(StrictTriple lt found gt) = splitMemberSet x l
          !gt' = Set.link y gt r
       in StrictTriple lt found gt'
    GT ->
      let !(StrictTriple lt found gt) = splitMemberSet x r
          !lt' = Set.link y l lt
       in StrictTriple lt' found gt
    EQ -> StrictTriple l True r
{-# INLINEABLE splitMemberSet #-}

-- | intersetDomP p m1 m2 == Keep the key and value from m2, iff (the key is in the dom of m1) && ((p key value) is true)
intersectDomP :: Ord k => (k -> v2 -> Bool) -> Map k v1 -> Map k v2 -> Map k v2
intersectDomP _ Tip _ = Tip
intersectDomP _ _ Tip = Tip
intersectDomP p t1 (Bin _ k v l2 r2) =
  if mb && p k v
    then link k v l1l2 r1r2
    else link2 l1l2 r1r2
  where
    !(StrictTriple l1 mb r1) = splitMemberMap k t1
    !l1l2 = intersectDomP p l1 l2
    !r1r2 = intersectDomP p r1 r2
{-# INLINEABLE intersectDomP #-}

-- | - Similar to intersectDomP, except the Map returned has the same key as the first input map, rather than the second input map.
intersectDomPLeft :: Ord k => (k -> v2 -> Bool) -> Map k v1 -> Map k v2 -> Map k v1
intersectDomPLeft _ Tip _ = Tip
intersectDomPLeft _ _ Tip = Tip
intersectDomPLeft p (Bin _ k v1 l1 r1) t2 =
  case mb of
    Just v2 | p k v2 -> link k v1 l1l2 r1r2
    _other -> link2 l1l2 r1r2
  where
    !(l2, mb, r2) = Map.splitLookup k t2
    !l1l2 = intersectDomPLeft p l1 l2
    !r1r2 = intersectDomPLeft p r1 r2
{-# INLINEABLE intersectDomPLeft #-}

-- | - fold over the intersection of a Map and a Set
intersectMapSetFold :: Ord k => (k -> v -> ans -> ans) -> Map k v -> Set k -> ans -> ans
intersectMapSetFold _accum Tip _ !ans = ans
intersectMapSetFold _accum _ set !ans | Set.null set = ans
intersectMapSetFold accum (Bin _ k v l1 l2) set !ans =
  intersectMapSetFold accum l1 s1 (addKV k v (intersectMapSetFold accum l2 s2 ans))
  where
    (s1, found, s2) = Set.splitMember k set
    addKV k1 v1 !ans1 = if found then accum k1 v1 ans1 else ans1
{-# INLINEABLE intersectMapSetFold #-}

-- | Fold with 'accum' all those pairs in the map, not appearing in the set.
disjointMapSetFold :: Ord k => (k -> v -> ans -> ans) -> Map k v -> Set k -> ans -> ans
disjointMapSetFold _accum Tip _ !ans = ans
disjointMapSetFold accum m set !ans | Set.null set = Map.foldrWithKey' accum ans m
disjointMapSetFold accum (Bin _ k v l1 l2) set !ans =
  disjointMapSetFold accum l1 s1 (addKV k v (disjointMapSetFold accum l2 s2 ans))
  where
    (s1, found, s2) = Set.splitMember k set
    addKV k1 v1 !ans1 = if not found then accum k1 v1 ans1 else ans1
{-# INLINEABLE disjointMapSetFold #-}

-- =================================

-- This is a slightly adjusted version of `delete` from "containers"
extract# :: Ord k => k -> Map k a -> (# Maybe a, Map k a #)
extract# !k = go
  where
    go Tip = (# Nothing, Tip #)
    go t@(Bin _ kx x l r) =
      case compare k kx of
        LT
          | l' `ptrEq` l -> (# mVal, t #)
          | otherwise -> let !m = balanceR kx x l' r in (# mVal, m #)
          where
            !(# mVal, l' #) = go l
        GT
          | r' `ptrEq` r -> (# mVal, t #)
          | otherwise -> let !m = balanceL kx x l r' in (# mVal, m #)
          where
            !(# mVal, r' #) = go r
        EQ -> let !m = glue l r in (# Just x, m #)
{-# INLINE extract# #-}

-- | Just like `Map.delete`, but also returns the value if it was indeed deleted
-- from the map.
extract :: Ord k => k -> Map k b -> (Maybe b, Map k b)
extract k m =
  case extract# k m of
    (# Just v, m' #) -> (Just v, m')
    _ -> (Nothing, m)
{-# INLINE extract #-}

-- | Partition the `Map` according to keys in the `Set`. This is equivalent to:
--
-- > extractKeys m s === (withoutKeys m s, restrictKeys m s)
extractKeys :: Ord k => Map k a -> Set k -> (Map k a, Map k a)
extractKeys m s
  | Set.size s < 6 = extractKeysSmallSet m s -- See haddock for value 6
  | otherwise =
      case extractKeys# m s of
        (# w, r #) -> (w, r)
{-# INLINE extractKeys #-}

-- | It has been discovered expirementally through benchmarks that for small Set
-- size of under around 6 elements this function performs faster than
-- `extractKeys#`
extractKeysSmallSet :: Ord k => Map k a -> Set.Set k -> (Map k a, Map k a)
extractKeysSmallSet sm = Set.foldl' f (sm, Map.empty)
  where
    f acc@(without, restrict) k =
      case extract# k without of
        (# Just v, without' #) ->
          let !restrict' = Map.insert k v restrict
           in (without', restrict')
        _ -> acc
{-# INLINE extractKeysSmallSet #-}

-- | This function will produce exactly the same results as
-- `extractKeysSmallSet` for all inputs, but it performs better whenever the set
-- is big.
extractKeys# :: Ord k => Map k a -> Set k -> (# Map k a, Map k a #)
extractKeys# Tip _ = (# Tip, Tip #)
extractKeys# m Set.Tip = (# m, Tip #)
extractKeys# m@(Bin _ k x lm rm) s = (# w, r #)
  where
    !(StrictTriple ls b rs) = splitMemberSet k s
    !w
      | not b =
          if lmw `ptrEq` lm && rmw `ptrEq` rm
            then m
            else link k x lmw rmw
      | otherwise = link2 lmw rmw
    !r
      | b =
          if lmr `ptrEq` lm && rmr `ptrEq` rm
            then m
            else link k x lmr rmr
      | otherwise = link2 lmr rmr
    !(# lmw, lmr #) = extractKeys# lm ls
    !(# rmw, rmr #) = extractKeys# rm rs
{-# INLINEABLE extractKeys# #-}

-- | Convert any foldable data structure with keys to a Map. Implemented in terms of
-- `Map.fromList`, therefore last duplicate key wins.
fromKeys :: (Foldable f, Ord k) => (k -> v) -> f k -> Map k v
fromKeys f ks =
  -- Conversion implemented in terms of list instead of an Map.insert because fromList has
  -- a nice optimization for already sorted keys and with list fusion there should be no overhead
  Map.fromList [(k, f k) | k <- toList ks]
{-# INLINE [2] fromKeys #-}

{-# RULES "fromKeys/fromSet" [~2] fromKeys = Map.fromSet #-}

-- | Convert any foldable data structure with values to a Map. Implemented in terms of
-- `Map.fromList`, therefore last duplicate key wins.
fromElems ::
  (Foldable f, Ord k) =>
  -- | Function that will create a key from a value. Most common case is a hashing
  -- function.
  (v -> k) ->
  f v ->
  Map k v
fromElems f vs =
  -- Conversion implemented in terms of list instead of an Map.insert because fromList has
  -- a nice optimization for already sorted keys and with list fusion there should be no overhead
  Map.fromList [(f v, v) | v <- toList vs]
{-# INLINE fromElems #-}
