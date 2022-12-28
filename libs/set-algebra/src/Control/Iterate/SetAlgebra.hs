{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | Supports writing 'Set algebra' expressions, using overloaded set operations, that can
--   be applied to a variety of Basic types (Set, List, Map, etc). Also supports
--   a mechanism to evaluate them efficiently, choosing datatype specific algorithms.
--   This mechanism uses run-time rewrite rules to get the best algorithm. If there are
--   no rewrite rules for a specific expression, falls back to a less efficient generic algorithm.
module Control.Iterate.SetAlgebra where

import Control.Iterate.BaseTypes (
  BaseRep (..),
  Basic (..),
  Embed (..),
  Iter (..),
  Sett (..),
  Single (..),
  fromPairs,
 )
import Control.Iterate.Collect (Collect, front, one, rear, runCollect, when)
import Control.Iterate.Exp (
  Exp (..),
  Query (..),
  -- semantic meaning functions for Query

  andD,
  andPD,
  chainD,
  -- Operations on Fun

  constant,
  first,
  nEgate,
  plus,
  projD,
  rngElem,
  rngFst,
  rngSnd,
  second,
 )
import qualified Data.Map.Strict as Map
import Data.MapExtras (
  disjointMapSetFold,
  intersectDomP,
  intersectDomPLeft,
  intersectMapSetFold,
  keysEqual,
  noKeys,
 )
import qualified Data.Set as Set
import Data.UMap (Tag (..), View (..))
import qualified Data.UMap as UM
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
compile (Rng f) = (BaseD SetR (materialize SetR (loop query)), SetR) -- We really ought to memoize this. It might be computed many times.
  where
    query = fst (compile f)
    loop x = do (_k, v, x2) <- nxt x; front (v, ()) (loop x2)
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
run (BaseD SingleR x, SingleR) = x -- just return the data
run (BaseD ListR x, ListR) = x -- if the forms do not match.
run (BaseD _source x, ListR) = materialize ListR (fifo x) -- use fifo, since the order matters for Lists.
run (BaseD _source x, target) = materialize target (lifo x) -- use lifo, for others
run (other, ListR) = materialize ListR (fifo other) -- If it is a compound Iterator, for List, than materialize it using fifo
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

-- | cost O(min (size m) (size n) * log(max (size m) (size n))), BUT the constants are high, too slow except for small maps.
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
compute (DRestrict (Dom (Base (ViewR _) x)) (Base MapR y)) = UM.domRestrict x y
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
compute (KeyEqual (Dom (Base MapR m)) (Dom (Base MapR n))) = keysEqual m n
compute (KeyEqual (Base SetR (Sett m)) (Base SetR (Sett n))) = n == m
compute (KeyEqual (Base MapR xs) (Base SetR (Sett ys))) = Map.keysSet xs == ys
compute x = computeSlow x

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

-- | Turn a list of pairs into any 'Basic' type. The first argument is a 'BaseRep' which
--   chooses what Base type to construct.
-- The combine function comb = (\\ earlier later -> later) will let values
-- later in the list override ones earlier in the list, and comb =
-- (\\ earlier later -> earlier) will keep the value that appears first in the list
fromList :: Ord k => BaseRep f k v -> (v -> v -> v) -> [(k, v)] -> f k v
fromList MapR combine xs = Map.fromListWith combine xs
fromList ListR combine xs = fromPairs combine xs
fromList SetR combine xs = foldr (addp combine) (Sett (Set.empty)) xs
fromList SingleR combine xs = foldr (addp combine) Fail xs
fromList (ViewR Rew) combine xs = foldr (addp combine) (Rewards UM.empty) xs
fromList (ViewR Del) combine xs = foldr (addp combine) (Delegations UM.empty) xs
fromList (ViewR Ptr) combine xs = foldr (addp combine) (Ptrs UM.empty) xs

-- =======================================================================================

-- | A witness (BaseRep) can be used to materialize a (Collect k v) into the type witnessed by the BaseRep.
-- Recall a (Collect k v) has no intrinsic type (it is just an ABSTRACT sequence of tuples), so
-- the witness describes how to turn them into the chosen datatype. Note that materialize is meant
-- to be applied to a collection built by iterating over a Query. This produces the keys in
-- ascending order, with no duplicate keys. So we do not need to specify how to merge duplicate values.
materialize :: (Ord k) => BaseRep f k v -> Collect (k, v) -> f k v
materialize ListR x = fromPairs (\l _r -> l) (runCollect x [] (:))
materialize MapR x = runCollect x Map.empty (\(k, v) ans -> Map.insert k v ans)
materialize SetR x = Sett (runCollect x Set.empty (\(k, _) ans -> Set.insert k ans))
materialize SingleR x = runCollect x Fail (\(k, v) _ignore -> Single k v)
materialize (ViewR Rew) x = runCollect x (Rewards UM.empty) (\(k, v) ans -> UM.insert' k v ans)
materialize (ViewR Del) x = runCollect x (Delegations UM.empty) (\(k, v) ans -> UM.insert' k v ans)
materialize (ViewR Ptr) x = runCollect x (Ptrs UM.empty) (\(k, v) ans -> UM.insert' k v ans)

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
