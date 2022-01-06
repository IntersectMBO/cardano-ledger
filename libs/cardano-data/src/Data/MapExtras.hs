{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.MapExtras where

import qualified Data.Map as Map
import Data.Map.Internal (Map (..), link, link2)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#, (==#))

-- ===========================================================
-- Some times we need to write our own version of functions
-- over  Map.Map that do not appear in the library
-- For example
-- 1) version of Map.withoutKeys where both parts are Map.Map
-- 2) Comparing that two maps have exactly the same set of keys
-- 3) The intersection of two maps guarded by a predicate.
--    ((dom stkcred) ◁ deleg) ▷ (dom stpool))   ==>
--    intersectDomP (\ k v -> Map.member v stpool) stkcred deleg
-- ============================================================

noKeys :: Ord k => Map k a -> Map k b -> Map k a
noKeys Tip _ = Tip
noKeys m Tip = m
noKeys m (Bin _ k _ ls rs) = case Map.split k m of
  (lm, rm) -> link2 lm' rm' -- We know `k` is not in either `lm` or `rm`
    where
      !lm' = noKeys lm ls
      !rm' = noKeys rm rs
{-# INLINEABLE noKeys #-}

-- This version benchmarks better than the following three versions, by almost a factor of 4, at Trees with 100 to 100,000 pairs
-- keysEqual2 x y = Map.foldrWithKey' (\ k v ans -> k:ans) [] x == Map.foldrWithKey' (\ k v ans -> k:ans) [] y
-- keysEqual3 x y = Map.keysSet x == Map.keysSet y
-- keysEqual4 x y = Map.keys x == Map.keys y
-- This is a type specific version of sameDomain

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects.)
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}

keysEqual :: Ord k => Map k v1 -> Map k v2 -> Bool
keysEqual Tip Tip = True
keysEqual Tip (Bin _ _ _ _ _) = False
keysEqual (Bin _ _ _ _ _) Tip = False
keysEqual m (Bin _ k _ ls rs) =
  case splitMember k m of
    (lm, True, rm) -> keysEqual ls lm && keysEqual rs rm
    _ -> False

-- | A variant of 'splitLookup' that indicates only whether the
-- key was present, rather than producing its value. This is used to
-- implement 'keysEqual' to avoid allocating unnecessary 'Just'
-- constructors.
splitMember :: Ord k => k -> Map k a -> (Map k a, Bool, Map k a)
splitMember k0 m = case go k0 m of
  StrictTriple l mv r -> (l, mv, r)
  where
    go :: Ord k => k -> Map k a -> StrictTriple (Map k a) Bool (Map k a)
    go !k t =
      case t of
        Tip -> StrictTriple Tip False Tip
        Bin _ kx x l r -> case compare k kx of
          LT ->
            let StrictTriple lt z gt = go k l
                !gt' = link kx x gt r
             in StrictTriple lt z gt'
          GT ->
            let StrictTriple lt z gt = go k r
                !lt' = link kx x l lt
             in StrictTriple lt' z gt
          EQ -> StrictTriple l True r
{-# INLINEABLE splitMember #-}

data StrictTriple a b c = StrictTriple !a !b !c

-- | intersetDomP p m1 m2 == Keep the key and value from m2, iff (the key is in the dom of m1) && ((p key value) is true)
intersectDomP :: Ord k => (k -> v2 -> Bool) -> Map k v1 -> Map k v2 -> Map k v2
intersectDomP _ Tip _ = Tip
intersectDomP _ _ Tip = Tip
intersectDomP p t1 (Bin _ k v l2 r2) =
  if mb && (p k v)
    then link k v l1l2 r1r2
    else link2 l1l2 r1r2
  where
    !(l1, mb, r1) = splitMember k t1
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

-- =================================

intersectWhen1 :: Ord k => (k -> u -> v -> Bool) -> Map k u -> Map k v -> Map k u
intersectWhen1 p x y = Map.mergeWithKey (\k u v -> if p k u v then Just u else Nothing) (const Map.empty) (const Map.empty) x y

intersectWhen2 :: Ord k => (k -> u -> v -> Bool) -> Map k u -> Map k v -> Map k v
intersectWhen2 p x y = Map.mergeWithKey (\k u v -> if p k u v then Just v else Nothing) (const Map.empty) (const Map.empty) x y

filterMaybe :: (k -> a -> Maybe b) -> Map k a -> Map k b
filterMaybe _ Tip = Tip
filterMaybe p (Bin _ kx x l r) =
  case p kx x of
    Nothing -> link2 pl pr
    Just b -> link kx b pl pr
  where
    !pl = filterMaybe p l
    !pr = filterMaybe p r
