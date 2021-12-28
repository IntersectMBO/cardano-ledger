{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Supports writing 'Set algebra' expressions, using overloaded set operations, that can
--   be applied to a variety of Basic types (Set, List, Map, BiMap etc). Also supports
--   a mechanism to evaluate them efficiently, choosing datatype specific algorithms.
--   This mechanism uses run-time rewrite rules to get the best algorithm. If there are
--   no rewrite rules for a specific expression, falls back to a less efficient generic algorithm.
module Control.Iterate.SetAlgebra where

import Control.Iterate.BaseTypes (BaseRep (..), Basic (..), Embed (..), Iter (..), Sett (..), Single (..), fromPairs)
import Control.Iterate.Collect (Collect, front, one, rear, runCollect, when)
import Control.Iterate.Exp
  ( Exp (..),
    Query (..),
    -- semantic meaning functions for Query

    andD,
    andPD,
    chainD,
    -- Operations on Fun

    constant,
    first,
    materialize,
    nEgate,
    plus,
    projD,
    rngElem,
    rngFst,
    rngSnd,
    rngStep,
    second,
  )
import Data.BiMap (BiMap (..), biMapFromList, removeval)
import Data.Compact.SplitMap
  ( filterWithKey,
    foldlWithKey',
    intersectMapSplit,
    intersectSplitMap,
    intersection,
    member,
    restrictKeysSet,
    withoutKeysMap,
    withoutKeysSet,
    withoutKeysSplit,
  )
import qualified Data.Compact.SplitMap as Split
import Data.Map.Internal (Map (..), link, link2)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (lookup)

-- ===============================================================================

-- | Compile the (Exp (f k v)) to a Query iterator, and a BaseRep that indicates
--   how to materialize the iterator to the correct type. Recall the iterator
--   can be used to constuct many things using runCollect, but here we want
--   to materialize it to the same type as the (Exp (f k v)), i.e. (f k v).
compile :: Exp (f k v) -> (Query k v, BaseRep f k v)
compile (Base rep relation) = (BaseD rep relation, rep)
compile (Singleton d r) = (BaseD SingleR (Single d r), SingleR)
compile (SetSingleton d) = (BaseD SingleR (SetSingle d), SingleR)
compile (Dom (Base SetR rel)) = (BaseD SetR rel, SetR)
compile (Dom (Singleton k _v)) = (BaseD SetR (Sett (Set.singleton k)), SetR)
compile (Dom (SetSingleton k)) = (BaseD SetR (Sett (Set.singleton k)), SetR)
compile (Dom x) = (projD (fst (compile x)) (constant ()), SetR)
compile (Rng (Base SetR _rel)) = (BaseD SetR (Sett (Set.singleton ())), SetR)
compile (Rng (Singleton _k v)) = (BaseD SetR (Sett (Set.singleton v)), SetR)
compile (Rng (SetSingleton _k)) = (BaseD SetR (Sett (Set.singleton ())), SetR)
compile (Rng f) = (BaseD SetR (rngStep (fst (compile f))), SetR) -- We really ought to memoize this. It might be computed many times.
compile (DRestrict set rel) = (projD (andD (fst (compile set)) reld) rngSnd, rep)
  where
    (reld, rep) = compile rel
compile (DExclude set rel) = (DiffD reld (fst (compile set)), rep)
  where
    (reld, rep) = compile rel
compile (RRestrict rel set) =
  case (compile rel, compile set) of
    ((reld, rep), (BaseD _ x, _)) -> (GuardD reld (rngElem x), rep)
    ((reld, rep), (setd, _)) -> (chainD reld setd rngFst, rep)
compile (RExclude rel set) =
  case (compile rel, compile set) of
    ((reld, rep), (BaseD _ x, _)) -> (GuardD reld (nEgate (rngElem x)), rep)
    ((reld, rep), _) -> (GuardD reld (nEgate (rngElem (compute set))), rep) -- This could be expensive
compile (UnionOverrideLeft rel1 rel2) = (OrD rel1d (fst (compile rel2)) first, rep) -- first uses value from rel1 to override value from rel2
  where
    (rel1d, rep) = compile rel1
compile (UnionOverrideRight rel1 rel2) = (OrD rel1d (fst (compile rel2)) second, rep) -- second uses value from rel2 to override value from rel1
  where
    (rel1d, rep) = compile rel1
compile (UnionPlus rel1 rel2) = (OrD rel1d (fst (compile rel2)) plus, rep)
  where
    (rel1d, rep) = compile rel1
compile (Intersect rel1 rel2) = (andPD (fst (compile rel1)) (fst (compile rel2)) (constant ()), SetR)
compile (SetDiff rel1 rel2) = (DiffD rel1d (fst (compile rel2)), rep)
  where
    (rel1d, rep) = compile rel1

compileSubterm :: Exp a -> Exp (f k v) -> Query k v
compileSubterm _whole sub = fst (compile sub)

-- ===========================================================================
-- The second part of the generic algorithm is to run the compiled code.
-- This involves materiaizing every sub-expression, and then applying generic
-- operations to the sub-expressions to get the answer.  Some Exp's (ones built
-- with (Base baserep value) are aready materialized, so they only need be
-- transformed to the target type. If the target type, is the same type as the real
-- 'value' stored in (Base baserep value). Then this is a no-op, if not use the
-- function (materialize :: Ord k => BaseRep f k v -> Collect (k, v) -> f k v) to
-- convert it.
-- ===========================================================================

run :: (Ord k) => (Query k v, BaseRep f k v) -> f k v
run (BaseD SetR x, SetR) = x -- If it is already data (BaseD)
run (BaseD MapR x, MapR) = x -- and in the right form (the BaseRep's match)
run (BaseD SplitR x, SplitR) = x
run (BaseD SingleR x, SingleR) = x -- just return the data
run (BaseD BiMapR x, BiMapR) = x -- only need to materialize data
run (BaseD ListR x, ListR) = x -- if the forms do not match.
run (BaseD _source x, ListR) = materialize ListR (fifo x) -- use fifo, since the order matters for Lists.
run (BaseD _source x, target) = materialize target (lifo x) -- use lifo, for others
run (other, ListR) = materialize ListR (fifo other) -- If it is a compund Iterator, for List, than materialize it using fifo
run (other, target) = materialize target (lifo other) -- If it is a compund Iterator, for anything else than materialize it using lifo

testing :: Bool
testing = False

runBoolExp :: Exp Bool -> Bool
runBoolExp e =
  if testing
    then error ("In Testing mode, SetAlgebra expression: " ++ show e ++ " falls through to slow mode.")
    else runBool e

runSetExp :: Ord k => Exp (f k v) -> f k v
runSetExp e =
  if testing
    then error ("In Testing mode, SetAlgebra expression: " ++ show e ++ " falls through to slow mode.")
    else run (compile e)

-- The following ar only for use in the SetAlgebra internal tests

runSet :: Ord k => Exp (f k v) -> f k v
runSet e = run (compile e)

runBool :: Exp Bool -> Bool
runBool (Elem k v) = haskey k (compute v)
runBool (NotElem k set) = not $ haskey k (compute set)
runBool (w@(KeyEqual x y)) = sameDomain (compileSubterm w x) (compileSubterm w y)
runBool (w@(Subset x y)) = runCollect (lifo left) True (\(k, _v) ans -> haskey k right && ans)
  where
    left = compileSubterm w x
    right = compileSubterm w y

-- ==============================================================================================
-- The faster strategy involves applying (type-specific) rewrite rules using the
-- function (compute :: Exp t -> t), This pattern matches against the GADT constructors
-- so this allows runtime choice of type specific algortihms of Exp
-- Evaluate an (Exp t) into real data of type t. Try domain and type specific algorithms first,
-- and if those fail. Compile the formula as an iterator, then run the iterator to get an answer.
-- Here are some sample of the type specific algorithms we incorporate
--  x  ∈ (dom y)            haskey
--  x  ∉ (dom y)            not . haskey
-- x ∪ (singleton y)        addpair
-- (Set.singleton x) ⋪ y    removekey
-- x ⋫ (Set.singleton y)    easy on Bimap  remove val
-- (dom x) ⊆ (dom y)
-- ===============================================================================================

compute :: Exp t -> t
compute (Base _rep relation) = relation
compute (Dom (Base SetR rel)) = rel
compute (Dom (Base MapR x)) = Sett (Map.keysSet x)
compute (Dom (Singleton k _v)) = Sett (Set.singleton k)
compute (Dom (SetSingleton k)) = Sett (Set.singleton k)
compute (Dom (Base _rep rel)) = Sett (domain rel)
-- (dom (Map(62)? ▷ (setSingleton _ )))
compute (Dom (RRestrict (Base MapR xs) (SetSingleton v))) = Sett (Map.foldlWithKey' accum Set.empty xs)
  where
    accum ans k u = if u == v then Set.insert k ans else ans
compute (Dom (RRestrict (Base MapR xs) (Base SetR (Sett set)))) = Sett (Map.foldlWithKey' accum Set.empty xs)
  where
    accum ans k u = if Set.member u set then Set.insert k ans else ans
compute (Dom (RExclude (Base MapR xs) (SetSingleton v))) = Sett (Map.foldlWithKey' accum Set.empty xs)
  where
    accum ans k u = if not (u == v) then Set.insert k ans else ans
compute (Dom (RExclude (Base MapR xs) (Base SetR (Sett set)))) = Sett (Map.foldlWithKey' accum Set.empty xs)
  where
    accum ans k u = if not (Set.member u set) then Set.insert k ans else ans
compute (Dom (DRestrict (SetSingleton v) (Base MapR xs))) = Sett (intersectMapSetFold accum xs (Set.singleton v) Set.empty)
  where
    accum k _u ans = Set.insert k ans
compute (Dom (DRestrict (Base SetR (Sett set)) (Base MapR xs))) = Sett (intersectMapSetFold accum xs set Set.empty)
  where
    accum k _u ans = Set.insert k ans
compute (Dom (DExclude (SetSingleton v) (Base MapR xs))) = Sett (disjointMapSetFold accum xs (Set.singleton v) Set.empty)
  where
    accum k _u ans = Set.insert k ans
compute (Dom (DExclude (Base SetR (Sett set)) (Base MapR xs))) = Sett (disjointMapSetFold accum xs set Set.empty)
  where
    accum k _u ans = Set.insert k ans
compute (Rng (Base SetR _rel)) = Sett (Set.singleton ())
compute (Rng (Singleton _k v)) = Sett (Set.singleton v)
compute (Rng (SetSingleton _k)) = Sett (Set.singleton ())
compute (Rng (Base _rep rel)) = Sett (range rel)
compute (DRestrict (Base SetR (Sett set)) (Base MapR m)) = Map.restrictKeys m set
compute (DRestrict (SetSingleton k) (Base MapR m)) = Map.restrictKeys m (Set.singleton k)
compute (DRestrict (Singleton k _v) (Base MapR m)) = Map.restrictKeys m (Set.singleton k)
compute (DRestrict (Dom (Base MapR x)) (Base MapR y)) = Map.intersection y x
-- This case inspired by set expression in EpochBoundary.hs
-- (dom (delegs ▷ Set.singleton hk) ◁ stake) in EpochBoundart.hs
-- ((dom (Map(62)? ▷ (setSingleton _ ))) ◁ Map(63)?) which has this structure
-- materialize MapR (do { (x,y,z) <- delegs `domEq` stake; when (y==hk); one(x,z) })
compute (DRestrict (Dom (RRestrict (Base MapR delegs) (SetSingleton hk))) (Base MapR stake)) =
  intersectDomPLeft (\_k v2 -> v2 == hk) stake delegs
compute (DRestrict (Dom (RRestrict (Base MapR delegs) (Base _ rngf))) (Base MapR stake)) =
  intersectDomPLeft (\_k v2 -> haskey v2 rngf) stake delegs
compute (DRestrict set (Base MapR ys)) = Map.restrictKeys ys set2 -- Pay the cost of materializing set to use O(n* log n) restictKeys
  where
    Sett set2 = materialize SetR (lifo (compute set))
compute (DRestrict (Base SetR (Sett s1)) (Base SetR (Sett s2))) = Sett (Set.intersection s1 s2)
compute (DRestrict (Base SetR x1) (Base rep x2)) = materialize rep $ do (x, _, z) <- x1 `domEq` x2; one (x, z)
compute (DRestrict (Dom (Base _ x1)) (Base rep x2)) = materialize rep $ do (x, _, z) <- x1 `domEq` x2; one (x, z)
compute (DRestrict (SetSingleton k) (Base rep x2)) = materialize rep $ do (x, _, z) <- (SetSingle k) `domEq` x2; one (x, z)
compute (DRestrict (Dom (Singleton k _)) (Base rep x2)) = materialize rep $ do (x, _, z) <- (SetSingle k) `domEq` x2; one (x, z)
compute (DRestrict (Rng (Singleton _ v)) (Base rep x2)) = materialize rep $ do (x, _, z) <- (SetSingle v) `domEq` x2; one (x, z)
compute (DExclude (SetSingleton n) (Base MapR m)) = Map.withoutKeys m (Set.singleton n)
compute (DExclude (Dom (Singleton n _v)) (Base MapR m)) = Map.withoutKeys m (Set.singleton n)
compute (DExclude (Rng (Singleton _n v)) (Base MapR m)) = Map.withoutKeys m (Set.singleton v)
compute (DExclude (Base SetR (Sett x1)) (Base MapR x2)) = Map.withoutKeys x2 x1
compute (DExclude (Dom (Base MapR x1)) (Base MapR x2)) = noKeys x2 x1
compute (DExclude (SetSingleton k) (Base BiMapR x)) = removekey k x
compute (DExclude (Dom (Singleton k _)) (Base BiMapR x)) = removekey k x
compute (DExclude (Rng (Singleton _ v)) (Base BiMapR x)) = removekey v x
compute (RExclude (Base BiMapR x) (SetSingleton k)) = removeval k x
compute (RExclude (Base BiMapR x) (Dom (Singleton k _v))) = removeval k x
compute (RExclude (Base BiMapR x) (Rng (Singleton _k v))) = removeval v x
compute (RExclude (Base MapR xs) (Base SetR (Sett y))) = Map.filter (\x -> not (Set.member x y)) xs
compute (RExclude (Base MapR xs) (SetSingleton k)) = Map.filter (not . (== k)) xs
compute (RExclude (Base _rep lhs) (Base SetR (Sett rhs))) | Set.null rhs = lhs
compute (RExclude (Base _rep lhs) (Base SingleR Fail)) = lhs
compute (RExclude (Base rep lhs) y) =
  materialize rep $ do (a, b) <- lifo lhs; when (not (haskey b rhs)); one (a, b)
  where
    (rhs, _) = compile y

-- (dom (Map(16)? ▷ (setSingleton _ )))
compute (RRestrict (Base MapR xs) (SetSingleton k)) = Map.filter (\x -> x == k) xs
-- ((dom rewards' ◁ delegs) ▷ dom poolParams)  in LedgerState.hs
compute (RRestrict (DRestrict (Dom (Base MapR x)) (Base MapR y)) (Dom (Base MapR z))) = intersectDomP (\_k v -> Map.member v z) x y
compute (RRestrict (DRestrict (Dom (Base _r1 stkcreds)) (Base r2 delegs)) (Dom (Base _r3 stpools))) =
  materialize r2 $ do (x, _, y) <- stkcreds `domEq` delegs; y `element` stpools; one (x, y)
compute (Elem k (Dom (Base _rep x))) = haskey k x
compute (Elem k (Base _rep rel)) = haskey k rel
compute (Elem k (Dom (Singleton key _v))) = k == key
compute (Elem k (Rng (Singleton _ key))) = k == key
compute (Elem k (SetSingleton key)) = k == key
compute (Elem k (UnionOverrideLeft (Base SetR (Sett x)) (Base SetR (Sett y)))) = (Set.member k x || Set.member k y)
compute (Elem k (UnionOverrideRight (Base SetR (Sett x)) (Base SetR (Sett y)))) = (Set.member k x || Set.member k y)
compute (Elem k (UnionPlus (Base SetR (Sett x)) (Base SetR (Sett y)))) = (Set.member k x || Set.member k y)
compute (Elem k (Intersect (Base SetR (Sett x)) (Base SetR (Sett y)))) = (Set.member k x && Set.member k y)
compute (Elem k (DRestrict s1 m1)) = compute (Elem k s1) && compute (Elem k m1)
compute (Elem k (DExclude s1 m1)) = not (compute (Elem k s1)) && compute (Elem k m1)
compute (NotElem k (Dom (Base _rep x))) = not $ haskey k x
compute (NotElem k (Base _rep rel)) = not $ haskey k rel
compute (NotElem k (Dom (Singleton key _v))) = not $ k == key
compute (NotElem k (Rng (Singleton _ key))) = not $ k == key
compute (NotElem k (SetSingleton key)) = not $ k == key
compute (NotElem k (UnionOverrideLeft (Base SetR (Sett x)) (Base SetR (Sett y)))) = not (Set.member k x || Set.member k y)
compute (NotElem k (UnionOverrideRight (Base SetR (Sett x)) (Base SetR (Sett y)))) = not (Set.member k x || Set.member k y)
compute (NotElem k (UnionPlus (Base SetR (Sett x)) (Base SetR (Sett y)))) = not (Set.member k x || Set.member k y)
compute (NotElem k (Intersect (Base SetR (Sett x)) (Base SetR (Sett y)))) = not (Set.member k x && Set.member k y)
compute (Subset (Base SetR (Sett x)) (Base SetR (Sett y))) = Set.isSubsetOf x y
compute (Subset (Base SetR (Sett x)) (Base MapR y)) = all (`Map.member` y) x
compute (Subset (Base SetR (Sett x)) (Dom (Base MapR y))) = all (`Map.member` y) x
compute (Subset (Base MapR x) (Base MapR y)) = Map.foldrWithKey accum True x
  where
    accum k _a ans = Map.member k y && ans
compute (Subset (Dom (Base MapR x)) (Dom (Base MapR y))) = Map.foldrWithKey accum True x
  where
    accum k _a ans = Map.member k y && ans
compute (Intersect (Base SetR (Sett x)) (Base SetR (Sett y))) = Sett (Set.intersection x y)
compute (Intersect (Base MapR x) (Base MapR y)) = Sett (Map.keysSet (Map.intersection x y))
compute (SetDiff (Base SetR (Sett x)) (Base SetR (Sett y))) = Sett (Set.difference x y)
compute (SetDiff (Base SetR (Sett x)) (Base MapR y)) = Sett (Set.filter (\e -> not (Map.member e y)) x)
compute (SetDiff (Base SetR (Sett x)) (Dom (Base MapR y))) = Sett (Set.filter (\e -> not (Map.member e y)) x)
compute (SetDiff (Base MapR x) (Dom (Base MapR y))) = Map.difference x y
compute (SetDiff (Base MapR x) (Base MapR y)) = (Map.difference x y)
compute (SetDiff (Base MapR x) (Base SetR (Sett y))) = (Map.withoutKeys x y)
compute (UnionOverrideLeft (Base _rep x) (Singleton k v)) = addkv (k, v) x (\old _new -> old) -- The value on the left is preferred over the right, so 'addkv' chooses 'old'
compute (UnionOverrideLeft (Base MapR d0) (Base MapR d1)) = Map.union d0 d1 -- 'Map.union' is left biased, just what we want.
compute (UnionOverrideLeft (Base SetR (Sett x)) (Base SetR (Sett y))) = Sett (Set.union x y)
compute (UnionOverrideLeft (DExclude (SetSingleton k) (Base MapR xs)) (Base MapR ys)) = Map.union (Map.delete k xs) ys
compute (UnionOverrideLeft (DExclude (Base SetR (Sett s1)) (Base MapR m2)) (Base MapR m3)) = Map.union (Map.withoutKeys m2 s1) m3
compute (UnionOverrideRight (Base _rep x) (Singleton k v)) = addkv (k, v) x (\_old new -> new) -- The value on the right is preferred over the left, so 'addkv' chooses 'new'
compute (UnionOverrideRight (Base MapR d0) (Base MapR d1)) = Map.union d1 d0 -- we pass @d1@ as first argument, since 'Map.union' is left biased.
compute (UnionOverrideRight (Base SetR (Sett x)) (Base SetR (Sett y))) = Sett (Set.union x y)
compute (UnionPlus (Base MapR x) (Base MapR y)) = Map.unionWith (<>) x y
compute (UnionPlus (Base SetR (Sett x)) (Base SetR (Sett y))) = Sett (Set.union x y) -- Recall (Sett k):: f k (), so () <> () = ()
compute (Singleton k v) = Single k v
compute (SetSingleton k) = (SetSingle k)
compute (KeyEqual (Base MapR m) (Base MapR n)) = keysEqual m n
compute (KeyEqual (Base BiMapR (MkBiMap m _)) (Base BiMapR (MkBiMap n _))) = keysEqual m n
compute (KeyEqual (Dom (Base MapR m)) (Dom (Base MapR n))) = keysEqual m n
compute (KeyEqual (Dom (Base BiMapR (MkBiMap m _))) (Dom (Base BiMapR (MkBiMap n _)))) = keysEqual m n
compute (KeyEqual (Base SetR (Sett m)) (Base SetR (Sett n))) = n == m
compute (KeyEqual (Base MapR xs) (Base SetR (Sett ys))) = Map.keysSet xs == ys
compute x =
  case rewrite x of
    Just t -> t
    Nothing -> computeSlow x

eval :: Embed s t => Exp t -> s
eval x = fromBase (compute x)

computeSlow :: Exp t -> t
computeSlow (Base _ t) = t
computeSlow (e@(Dom _)) = runSetExp e
computeSlow (e@(Rng _)) = runSetExp e
computeSlow (e@(DRestrict _ _)) = runSetExp e
computeSlow (e@(DExclude _ _)) = runSetExp e
computeSlow (e@(RExclude _ _)) = runSetExp e
computeSlow (e@(RRestrict _ _)) = runSetExp e
computeSlow (e@(Elem _ _)) = runBoolExp e
computeSlow (e@(NotElem _ _)) = runBoolExp e
computeSlow (e@(Subset _ _)) = runBoolExp e
computeSlow (e@(Intersect _ _)) = runSetExp e
computeSlow (e@(SetDiff _ _)) = runSetExp e
computeSlow (e@(UnionOverrideLeft _ _)) = runSetExp e
computeSlow (e@(UnionOverrideRight _ _)) = runSetExp e
computeSlow (e@(UnionPlus _ _)) = runSetExp e
computeSlow (Singleton k v) = Single k v
computeSlow (SetSingleton k) = (SetSingle k)
computeSlow (e@(KeyEqual _ _)) = runBoolExp e

-- =========================================================================
-- Apply rewrite rules for SplitMap

rewrite :: Exp t -> Maybe t
rewrite (Base _rep relation) = Just relation
-- t01
rewrite (Dom (Base SplitR x)) = Just $ Sett (foldlWithKey' (\ans k _ -> Set.insert k ans) Set.empty x)
-- t02
rewrite (Dom (RRestrict (Base SplitR xs) (SetSingleton v))) = Just $ Sett (foldlWithKey' accum Set.empty xs)
  where
    accum ans k u = if u == v then Set.insert k ans else ans
-- t03
rewrite (Dom (RRestrict (Base SplitR xs) (Base SetR (Sett set)))) = Just $ Sett (foldlWithKey' accum Set.empty xs)
  where
    accum ans k u = if Set.member u set then Set.insert k ans else ans
-- t04
rewrite (Dom (RExclude (Base SplitR xs) (Base SetR (Sett set)))) = Just $ Sett (foldlWithKey' accum Set.empty xs)
  where
    accum ans k u = if not (Set.member u set) then Set.insert k ans else ans
-- t05
rewrite (Dom (RExclude (Base SplitR xs) (SetSingleton v))) = Just $ Sett (foldlWithKey' accum Set.empty xs)
  where
    accum ans k u = if not (u == v) then Set.insert k ans else ans
-- t06
rewrite (Dom (DRestrict (SetSingleton v) (Base SplitR xs))) =
  if member v xs
    then Just (Sett (Set.singleton v))
    else Just (Sett (Set.empty))
-- t07
rewrite (Dom (DRestrict (Base SetR (Sett set)) (Base SplitR xs))) = Just $ Sett (Set.foldl' accum Set.empty set)
  where
    accum ans k = if member k xs then Set.insert k ans else ans
-- t08
rewrite (Dom (DExclude (SetSingleton v) (Base SplitR xs))) = Just $ Sett (foldlWithKey' accum Set.empty xs)
  where
    accum ans k _v = if k == v then ans else Set.insert k ans
-- t09
rewrite (Dom (DExclude (Base SetR (Sett set)) (Base SplitR xs))) = Just $ Sett (foldlWithKey' accum Set.empty xs)
  where
    accum ans k _v = if Set.member k set then ans else Set.insert k ans
-- t10
rewrite (DRestrict (Base SetR (Sett set)) (Base SplitR m)) = Just $ restrictKeysSet m set
-- t11
rewrite (DRestrict (SetSingleton k) (Base SplitR m)) = Just $ restrictKeysSet m (Set.singleton k)
-- t12
rewrite (DRestrict (Singleton k _v) (Base SplitR m)) = Just $ restrictKeysSet m (Set.singleton k)
-- t13
rewrite (DRestrict (Dom (Base MapR x)) (Base SplitR y)) = Just $ intersectSplitMap always y x
  where
    always _ _ _ = True
-- t14
rewrite (DRestrict (Dom (Base SplitR x)) (Base MapR y)) = Just $ intersectMapSplit always y x
  where
    always _ _ _ = True
-- t15
rewrite (DRestrict (Dom (Base SplitR x)) (Base SplitR y)) = Just $ intersection y x
-- t16
rewrite (DRestrict (Dom (RRestrict (Base MapR delegs) (SetSingleton hk))) (Base SplitR stake)) =
  Just $
    intersectSplitMap (\_k _u v2 -> v2 == hk) stake delegs
-- t17
rewrite (DRestrict (Dom (RRestrict (Base MapR delegs) (Base _ rngf))) (Base SplitR stake)) =
  Just $
    intersectSplitMap (\_k _u v2 -> haskey v2 rngf) stake delegs
-- t18
rewrite (DRestrict set (Base SplitR ys)) = Just $ restrictKeysSet ys set2
  where
    -- Pay the cost of materializing set to use O(n* log n) restictKeys
    Sett set2 = materialize SetR (lifo (compute set))
-- t19
rewrite (DExclude (SetSingleton n) (Base SplitR m)) = Just $ withoutKeysSet m (Set.singleton n)
-- t20
rewrite (DExclude (Dom (Singleton n _v)) (Base SplitR m)) = Just $ withoutKeysSet m (Set.singleton n)
-- t21
rewrite (DExclude (Rng (Singleton _n v)) (Base SplitR m)) = Just $ withoutKeysSet m (Set.singleton v)
-- t22
rewrite (DExclude (Base SetR (Sett x1)) (Base SplitR x2)) = Just $ withoutKeysSet x2 x1
-- t23
rewrite (DExclude (Dom (Base MapR x1)) (Base SplitR x2)) = Just $ withoutKeysMap x2 x1
-- t24
rewrite (DExclude (Dom (Base SplitR x1)) (Base SplitR x2)) = Just $ withoutKeysSplit x2 x1
-- t25
rewrite (RExclude (Base SplitR xs) (SetSingleton u)) = Just $ filterWithKey (\_k v -> not (v == u)) xs
-- t26
rewrite (RExclude (Base SplitR xs) (Base SetR (Sett set))) = Just $ filterWithKey (\_k v -> not (Set.member v set)) xs
-- t27
rewrite (RRestrict (Base SplitR xs) (SetSingleton k)) = Just $ filterWithKey (\_ x -> x == k) xs
-- t28
rewrite (RRestrict (Base SplitR xs) (Base SetR (Sett s))) = Just $ filterWithKey (\_ x -> Set.member x s) xs
-- Additional rewrites to be added on a demand basis.
rewrite _ = Nothing

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

keysEqual :: Ord k => Map k v1 -> Map k v2 -> Bool
keysEqual Tip Tip = True
keysEqual Tip (Bin _ _ _ _ _) = False
keysEqual (Bin _ _ _ _ _) Tip = False
keysEqual m (Bin _ k _ ls rs) =
  case splitMember k m of
    (lm, True, rm) -> keysEqual ls lm && keysEqual rs rm
    _ -> False

-- cost O(min (size m) (size n) * log(max (size m) (size n))), BUT the constants are high, too slow except for small maps.
sameDomain :: (Ord k, Iter f, Iter g) => f k b -> g k c -> Bool
sameDomain m n = loop (hasNxt m) (hasNxt n)
  where
    loop (Just (k1, _, nextm)) (Just (k2, _, nextn)) =
      case compare k1 k2 of
        EQ -> loop (hasNxt nextm) (hasNxt nextn)
        LT -> False
        GT -> False
    loop Nothing Nothing = True
    loop _ _ = False

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

-- ==========================================================================
-- The most basic operation of iteration, where (Iter f) is to use the 'nxt'
-- operator on (f k v) to create a (Collect k v). The two possible
-- ways to produce their elements are in LIFO or FIFO order.
-- ===========================================================================

lifo :: Iter f => f k v -> Collect (k, v)
lifo x = do (k, v, x2) <- nxt x; front (k, v) (lifo x2)

fifo :: Iter f => f k v -> Collect (k, v)
fifo x = do (k, v, x2) <- nxt x; rear (fifo x2) (k, v)

-- ================================================================================
-- A witness (BaseRep) can be used to specifiy how to build a specifc datatype from
-- a CONCRETE sequence of tuples (a [(k,v)]). This is a way to import a type from from
--  a list. But unlike 'materialize' an arbitray [(k,v)] may have duplicate keys,
--  so when that happens, use 'combine' to merge the associated values.
-- ================================================================================

addp :: (Ord k, Basic f) => (v -> v -> v) -> (k, v) -> f k v -> f k v
addp combine (k, v) xs = addkv (k, v) xs combine

-- The combine function comb = (\ earlier later -> later) will let values
-- later in the list override ones earlier in the list, and comb =
-- (\ earlier later -> earlier) will keep the value that appears first in the list

fromList :: Ord k => BaseRep f k v -> (v -> v -> v) -> [(k, v)] -> f k v
fromList MapR combine xs = Map.fromListWith combine xs
fromList ListR combine xs = fromPairs combine xs
fromList SetR combine xs = foldr (addp combine) (Sett (Set.empty)) xs
fromList BiMapR combine xs = biMapFromList combine xs
fromList SingleR combine xs = foldr (addp combine) Fail xs
fromList SplitR combine xs = foldr (addp combine) Split.empty xs

-- =========================================================================================
-- Now we make an iterator that collects triples, on the intersection
-- of the domain of the two Iter types 'f' and 'g'. An answer of (k,b,c) means that
-- (k,b) is in m::f k a, and (k,c) is in n::g k c. All the other possible triples
-- are skipped over.  This is an instance of a thing called a "Generic Join"
-- See https://arxiv.org/pdf/1310.3314.pdf  or  http://personales.dcc.uchile.cl/~pbarcelo/ngo.pdf
-- The number of tuples it touches is proportional to the size of the output (modulo log factors).
-- It's cost is unrelated to the size of its inputs (modulo log factors)
-- This is a very specific version of the AndD compound iterator. It is used in the function 'eval'
-- =========================================================================================

(⨝) :: (Ord k, Iter f, Iter g) => f k b -> g k c -> Collect (k, b, c)
(⨝) = domEq

domEq :: (Ord k, Iter f, Iter g) => f k b -> g k c -> Collect (k, b, c)
domEq m n = do
  triplem <- nxt m
  triplen <- nxt n
  let loop (mt@(k1, b, nextm)) (nt@(k2, c, nextn)) =
        case compare k1 k2 of
          EQ -> front (k1, b, c) (domEq nextm nextn)
          LT -> do mt' <- lub k2 nextm; loop mt' nt
          GT -> do nt' <- lub k1 nextn; loop mt nt'
  loop triplem triplen

-- This is included here for the benchmark tests. It is much slower because it does not use lub.

domEqSlow :: (Ord k, Iter f, Iter g) => f k b -> g k c -> Collect (k, b, c)
domEqSlow m n = do
  triplem <- nxt m
  triplen <- nxt n
  let loop (mt@(k1, b, nextm)) (nt@(k2, c, nextn)) =
        case compare k1 k2 of
          EQ -> front (k1, b, c) (domEqSlow nextm nextn)
          LT -> do mt' <- nxt nextm; loop mt' nt
          GT -> do nt' <- nxt nextn; loop mt nt'
  loop triplem triplen

-- ================================================================================
