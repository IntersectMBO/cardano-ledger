{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Solver where

import Cardano.Ledger.Alonzo.Scripts.Data (hashData)
import Cardano.Ledger.BaseTypes (EpochNo (EpochNo), SlotNo (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Core (Era (..), hashScript)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (foldlM')
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Lens.Micro (Lens', (^.))
import qualified Lens.Micro as Lens
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (
  Adds (..),
  AddsSpec (..),
  Count (..),
  OrdCond (..),
  ScriptF (..),
  Sizeable (getSize),
  varOnLeft,
  varOnRight,
  varOnRightNeg,
  varOnRightSize,
 )
import Test.Cardano.Ledger.Constrained.Combinators (errorMess, genFromMap, itemFromSet, suchThatErr)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Rewrite (DependGraph (..), OrderInfo, compileGen, cpeq)
import Test.Cardano.Ledger.Constrained.Size (
  Size (..),
  genFromIntRange,
  genFromSize,
 )
import Test.Cardano.Ledger.Constrained.Spec (
  ElemSpec (..),
  ListSpec (..),
  MapSpec (..),
  PairSide (..),
  PairSpec (PairAny, PairSpec),
  RelSpec (..),
  RngSpec (..),
  SetSpec (..),
  genFromListSpec,
  genFromMapSpec,
  genFromSetSpec,
  mapSpec,
  relDisjoint,
  relEqual,
  relSubset,
  relSuperset,
  setSpec,
 )
import Test.Cardano.Ledger.Constrained.TypeRep (
  Rep (..),
  genRep,
  genSizedRep,
  hasEq,
  hasOrd,
  synopsis,
  testEql,
  (:~:) (Refl),
 )
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (Proof (..), Reflect (reify))
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck hiding (Fixed, getSize, total)

-- ===================================================================================
-- This is testing file, and sometimes it pays for solver to explain what it is doing
-- especially if it fails on some known input. This way the solver can leave a trace
-- of what it is doing, and why.

ifTrace :: Bool -> String -> a -> a
ifTrace traceOn message a = case traceOn of
  True -> trace message a
  False -> a

-- ==================================================
-- Computing if a type has instances

-- ============ Adds ==============

-- | Is there an Adds instance for 't'
hasAdds :: Rep era t -> (s t) -> Typed (HasConstraint Adds (s t))
hasAdds ExUnitsR x = pure $ With x
hasAdds Word64R x = pure $ With x
hasAdds IntR x = pure $ With x
hasAdds NaturalR x = pure $ With x
hasAdds RationalR x = pure $ With x
hasAdds CoinR x = pure $ With x
hasAdds DeltaCoinR x = pure $ With x
hasAdds r _ = failT [show r ++ " does not have Adds instance."]

isAddsType :: forall era t. Rep era t -> Bool
isAddsType rep = case hasAdds rep rep of
  (Typed (Right (With _))) -> True
  (Typed (Left _)) -> False

-- ============= Count ================

-- | Is there an Count instance for 't'
hasCount :: Rep era t -> s t -> Typed (HasConstraint Count (s t))
hasCount IntR x = pure $ With x
hasCount (ProtVerR _) x = pure $ With x
hasCount EpochR x = pure $ With x
hasCount SlotNoR x = pure $ With x
hasCount r _ = failT [show r ++ " does not have Count instance."]

isCountType :: forall era t. Rep era t -> Bool
isCountType rep = case hasCount rep rep of
  (Typed (Right (With _))) -> True
  (Typed (Left _)) -> False

-- ==================================================
-- Extras, simple helper functions

sameRep :: Rep era i -> Rep era j -> Typed (i :~: j)
sameRep r1 r2 = case testEql r1 r2 of
  Just x -> pure x
  Nothing -> failT ["Type error in sameRep:\n  " ++ show r1 ++ " =/=\n  " ++ show r2]

-- | Simplify and return with evidence that 'expr' has type 's'
simplifyAtType :: Rep era s -> Term era t -> Typed s
simplifyAtType r1 term = do
  t <- simplify term
  Refl <- sameRep r1 (termRep term)
  pure t

simplifySet :: Ord rng => Rep era rng -> Term era y -> Typed (HasConstraint Ord (Set rng))
simplifySet r1 term = do
  x <- simplify term
  Refl <- sameRep (SetR r1) (termRep term)
  pure (With x)

simplifyList :: Rep era rng -> Term era y -> Typed (HasConstraint Eq [rng])
simplifyList r1 term = do
  x <- simplify term
  Refl <- sameRep (ListR r1) (termRep term)
  hasEq r1 x

-- | Is the Sum a variable (of a map). Only SumMap and Project store maps.
isMapVar :: Name era -> Sum era c -> Bool
isMapVar n1 (SumMap (Var v2)) = n1 == Name v2
isMapVar n1 (ProjMap _ _ (Var v2)) = n1 == Name v2
isMapVar _ _ = False

exactlyOne :: (a -> Bool) -> [a] -> Bool
exactlyOne pp xs = 1 == length (filter pp xs)

-- | Make a generator for a Map type when there is a Projection from the domain of the map.
--   This has been superceeded by the RelLens  RelSpec
projOnDom ::
  forall era a dom rng.
  (Era era, Ord dom) =>
  Set a ->
  Lens' dom a ->
  Rep era dom ->
  Rep era rng ->
  Gen (Map dom rng)
projOnDom setA lensDomA repDom repRng = do
  ds <- mapM genThenOverwriteA (Set.toList setA)
  pairs <- mapM (\d -> do r <- genRep repRng; pure (d, r)) ds
  pure (Map.fromList pairs)
  where
    genThenOverwriteA a = Lens.set lensDomA a <$> genRep repDom

atLeast :: (Era era, Adds c) => Rep era c -> c -> Gen c
atLeast rep c = add c <$> genRep rep

-- ================================================================
-- Solver for variables of type (Map dom rng)
solveMap :: forall dom rng era. Era era => V era (Map dom rng) -> Pred era -> Typed (MapSpec era dom rng)
solveMap v1@(V _ r@(MapR dom rng) _) predicate = explain msg $ case predicate of
  (Sized (Lit SizeR sz) (Var v2))
    | Name v1 == Name v2 -> mapSpec sz RelAny PairAny RngAny
  (Sized (Lit SizeR sz) (Dom (Var v2)))
    | Name v1 == Name v2 -> mapSpec sz RelAny PairAny RngAny
  (Var v2 :=: expr) | Name v1 == Name v2 -> do
    m1 <- simplifyAtType r expr
    With _ <- hasEq rng rng
    mapSpec SzAny (relEqual dom (Map.keysSet m1)) PairAny (RngElem rng (Map.elems m1))
  (expr :=: v2@(Var _)) -> solveMap v1 (v2 :=: expr)
  (Elems (Var v2@(V _ r2 _)) :=: expr) | Name v1 == Name v2 -> do
    Refl <- sameRep r r2
    l <- simplifyAtType (ListR rng) expr
    mapSpec (SzExact (length l)) RelAny PairAny (RngElem rng l)
  (expr :=: Elems v2) -> solveMap v1 (Elems v2 :=: expr)
  (expr1 :=: Restrict expr2 (Var v2@(V _ (MapR a _) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep a dom
    val1 <- simplify expr1
    val2 <- simplify expr2
    mapSpec (SzLeast (Map.size val1)) (relSuperset dom val2) PairAny RngAny
  (Restrict expr1 expr2 :=: expr3) -> solveMap v1 (expr3 :=: Restrict expr1 expr2)
  (ProjS lensbt trep (Dom (Var v2@(V _ (MapR brep _) _))) :=: Lit (SetR drep) x) | Name v1 == Name v2 -> do
    Refl <- sameRep dom brep
    mapSpec (SzExact (Set.size x)) (RelLens lensbt dom trep (relEqual drep x)) PairAny RngAny
  (ProjS lensbt trep (Dom (Var v2@(V _ (MapR brep _) _))) :=: Dom (Lit (MapR drep _) x))
    | Name v1 == Name v2 -> do
        Refl <- sameRep dom brep
        mapSpec (SzExact (Map.size x)) (RelLens lensbt dom trep (relEqual drep (Map.keysSet x))) PairAny RngAny
  (ProjS lensbt trep (Dom (Var v2@(V _ (MapR brep _) _))) `Subset` Dom (Lit (MapR drep _) x))
    | Name v1 == Name v2 -> do
        Refl <- sameRep dom brep
        mapSpec (SzMost (Map.size x)) (RelLens lensbt dom trep (relSubset drep (Map.keysSet x))) PairAny RngAny
  (Lit (SetR drep) x) :=: ProjS lensbt trep (Dom (Var v2@(V _ (MapR brep _) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom brep
    mapSpec (SzExact (Set.size x)) (RelLens lensbt dom trep (relEqual drep x)) PairAny RngAny
  (expr :=: Rng (Var v2))
    | Name v1 == Name v2 -> do
        s <- simplify expr
        let SetR a = termRep expr
        Refl <- sameRep rng a
        mapSpec (SzLeast (Set.size s)) RelAny PairAny (RngRel (relEqual rng s))
  (Rng (Var v2) :=: expr)
    | Name v1 == Name v2 -> do
        s <- simplify expr
        let SetR a = termRep expr
        Refl <- sameRep rng a
        mapSpec (SzLeast (Set.size s)) RelAny PairAny (RngRel (relEqual rng s))
  (Rng (Var v2) `Subset` expr)
    | Name v1 == Name v2 -> do
        With _ <- hasOrd rng rng
        With n <- simplifySet rng expr
        mapSpec SzAny RelAny PairAny (RngRel (relSubset rng n))
  (expr `Subset` Rng (Var v2))
    | Name v1 == Name v2 -> do
        With _ <- hasOrd rng rng
        With n <- simplifySet rng expr
        mapSpec SzAny RelAny PairAny (RngRel (relSuperset rng n))
  (Rng expr `Subset` (Var v2))
    | Name v1 == Name v2 -> do
        With _ <- hasOrd rng rng
        With n <- simplifySet rng expr
        mapSpec SzAny RelAny PairAny (RngRel (relSuperset rng n))
  (expr :=: Dom (Var v2))
    | Name v1 == Name v2 -> do
        let SetR a = termRep expr
        Refl <- sameRep dom a
        mm <- simplify expr
        mapSpec (SzExact (Set.size mm)) (relSubset dom mm) PairAny RngAny
  (Dom (Var v2) :=: expr)
    | Name v1 == Name v2 -> do
        let SetR a = termRep expr
        Refl <- sameRep dom a
        mm <- simplify expr
        mapSpec (SzExact (Set.size mm)) (relSubset dom mm) PairAny RngAny
  (Dom (Var v2) `Subset` expr)
    | Name v1 == Name v2 -> do
        With _ <- hasOrd dom dom
        With n <- simplifySet dom expr
        mapSpec (SzMost (Set.size n)) (relSubset dom n) PairAny RngAny
  (expr `Subset` Dom (Var v2))
    | Name v1 == Name v2 -> do
        With _ <- hasOrd dom dom
        With n <- simplifySet dom expr
        mapSpec (SzLeast (Set.size n)) (relSuperset dom n) PairAny RngAny
  (SumsTo small expr cond xs) | exactlyOne (isMapVar (Name v1)) xs -> do
    t <- simplify expr
    rngspec <- solveMapSummands (direct small) t [msg] cond v1 zero xs
    mapSpec SzAny RelAny PairAny rngspec
  (Random (Var v2)) | Name v1 == Name v2 -> mapSpec SzAny RelAny PairAny RngAny
  (Sized (Size sz) (Var v2)) | Name v1 == Name v2 -> mapSpec sz RelAny PairAny RngAny
  (Disjoint expr (Dom (Var v2))) | Name v1 == Name v2 -> do
    With set <- simplifySet dom expr
    mapSpec SzAny (relDisjoint dom set) PairAny RngAny
  (Disjoint (Dom (Var v2)) expr) | Name v1 == Name v2 -> do
    With set <- simplifySet dom expr
    mapSpec SzAny (relDisjoint dom set) PairAny RngAny
  (Disjoint expr (Rng (Var v2))) | Name v1 == Name v2 -> do
    With _ <- hasOrd rng rng
    With set <- simplifySet rng expr
    mapSpec SzAny RelAny PairAny (RngRel $ relDisjoint rng set)
  (Disjoint (Rng (Var v2)) expr) | Name v1 == Name v2 -> do
    With _ <- hasOrd rng rng
    With set <- simplifySet rng expr
    mapSpec SzAny RelAny PairAny (RngRel $ relDisjoint rng set)
  (Member (Left (HashS expr)) (Dom (Var v2@(V _ (MapR a _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom a
    ScriptF _ x <- simplifyAtType (ScriptR reify) expr
    let hash = hashScript @era x
    mapSpec (SzLeast 1) (relSuperset dom (Set.singleton hash)) PairAny RngAny
  (Member (Right (HashS expr)) (Dom (Var v2@(V _ (MapR a _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom a
    ScriptF _ x <- simplifyAtType (ScriptR reify) expr
    let hash = hashScript @era x
    mapSpec (SzLeast 1) (relSuperset dom (Set.singleton hash)) PairAny RngAny
  (Member (Left (HashD expr)) (Dom (Var v2@(V _ (MapR a _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom a
    x <- simplifyAtType DataR expr
    let hash = hashData @era x
    mapSpec (SzLeast 1) (relSuperset dom (Set.singleton hash)) PairAny RngAny
  (Member (Right (HashD expr)) (Dom (Var v2@(V _ (MapR a _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom a
    x <- simplifyAtType DataR expr
    let hash = hashData @era x
    mapSpec (SzLeast 1) (relSuperset dom (Set.singleton hash)) PairAny RngAny
  (Member expr (Dom (Var v2@(V _ (MapR a _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom a
    x <- simplify (direct expr)
    mapSpec (SzLeast 1) (relSuperset dom (Set.singleton x)) PairAny RngAny
  (Member expr (Rng (Var v2@(V _ (MapR _ b) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep rng b
    x <- simplify (direct expr)
    mapSpec (SzLeast 1) RelAny PairAny (RngRel (relSuperset rng (Set.singleton x)))
  (NotMember expr (Dom (Var v2@(V _ (MapR a _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom a
    x <- simplify expr
    mapSpec SzAny (relDisjoint dom (Set.singleton x)) PairAny RngAny
  (NotMember expr (Rng (Var v2@(V _ (MapR _ b) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep rng b
    x <- simplify expr
    mapSpec SzAny RelAny PairAny (RngRel (relDisjoint rng (Set.singleton x)))
  (MapMember exprK exprV (Left (Var v2@(V _ (MapR dom2 rng2) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom dom2
    Refl <- sameRep rng rng2
    k <- simplify exprK
    v <- simplify exprV
    mapSpec
      (SzLeast 1)
      (relSuperset dom (Set.singleton k))
      (PairSpec dom rng VarOnRight (Map.singleton k v))
      (RngRel (relSuperset rng (Set.singleton v)))
  (MapMember exprK exprV (Right (Var v2@(V _ (MapR dom2 rng2) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom dom2
    Refl <- sameRep rng rng2
    k <- simplify exprK
    v <- simplify exprV
    mapSpec
      (SzLeast 1)
      (relSuperset dom (Set.singleton k))
      (PairSpec dom rng VarOnRight (Map.singleton k v))
      (RngRel (relSuperset rng (Set.singleton v)))
  (List (Var v2@(V _ r2 _)) xs) | Name v1 == Name v2 -> do
    Refl <- sameRep r r2
    With _ <- hasOrd rng rng
    let PairR dl rl = tsRep r2
    ys <- mapM (simplifyAtType (PairR dl rl)) xs
    let (ds, rs) = unzip ys
    mapSpec
      (SzExact (length ys))
      (relEqual dl (makeFromList ds))
      (PairSpec dom rng VarOnRight (Map.fromList ys))
      (RngRel (relEqual rl (makeFromList rs)))
  (SubMap subMapExpr (Var v2@(V _ (MapR dom2 rng2) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom dom2
    Refl <- sameRep rng rng2
    m <- simplify subMapExpr
    mapSpec
      (SzLeast (Map.size m))
      (relSuperset dom (Map.keysSet m))
      (PairSpec dom rng VarOnRight m)
      (RngRel (relSuperset rng (Set.fromList (Map.elems m))))
  (SubMap (Var v2@(V _ (MapR dom2 rng2) _)) subMapExpr) | Name v1 == Name v2 -> do
    Refl <- sameRep dom dom2
    Refl <- sameRep rng rng2
    m <- simplify subMapExpr
    mapSpec
      (SzMost (Map.size m))
      (relSubset dom (Map.keysSet m))
      (PairSpec dom rng VarOnLeft m)
      (RngRel (relSubset rng (Set.fromList (Map.elems m))))
  other -> failT ["Cannot solve map condition: " ++ show other]
  where
    msg = ("Solving for " ++ show v1 ++ " Predicate \n   " ++ show predicate)

-- | We are solving for a (V era (Map d r)). This must occurr exactly once in the [Sum era c]
--   That can only happen in a (RngSum cond c) or a (RngProj cond rep c) constructor of 'Sum'
--   Because we don't know if 'c' can have negative values, we do the summation as an Integer
solveMapSummands ::
  Adds c =>
  c ->
  c ->
  [String] ->
  OrdCond ->
  V era (Map dom rng) ->
  c ->
  [Sum era c] ->
  Typed (RngSpec era rng)
solveMapSummands small lhsC _ cond (V _ (MapR _ r) _) c [ProjMap _crep l (Var (V name (MapR _ r1) _))] = do
  Refl <- sameRep r r1
  pure (RngProj small r l (varOnRightSize lhsC cond c name))
solveMapSummands small lhsC _ cond (V _ (MapR _ r) _) c [SumMap (Var (V name (MapR _ r1) _))] = do
  Refl <- sameRep r r1
  pure (RngSum small (varOnRightSize lhsC cond c name))
solveMapSummands small lhsC msg cond v c (s : ss)
  | isMapVar (Name v) s =
      solveMapSummands small lhsC msg cond v c (ss ++ [s])
solveMapSummands small lhsC msg cond v c (s : ss) = do
  d <- summandAsInt s
  solveMapSummands small lhsC msg cond v (add c $ fromI ["solveMapSummands"] d) ss
solveMapSummands _ _ msg _ v _ [] = failT (("Does not have exactly one summand with variable " ++ show (Name v)) : msg)

solveMaps :: (Era era, Ord dom) => V era (Map dom rng) -> [Pred era] -> Typed (MapSpec era dom rng)
solveMaps v@(V _ (MapR _ _) _) cs =
  foldlM' accum (MapSpec SzAny RelAny PairAny RngAny) cs
  where
    accum spec cond = do
      condspec <- solveMap v cond
      liftT (spec <> condspec)

-- ===========================================================
-- Solving for variables with type Set

-- | Given a variable: 'v1', with a Set type, compute a SetSpec
--   which describes the constraints implied by the Pred 'predicate'
solveSet :: forall era a. V era (Set a) -> Pred era -> Typed (SetSpec era a)
solveSet v1@(V _ (SetR r) _) predicate = case predicate of
  (Sized (Size sz) (Var v2)) | Name v1 == Name v2 -> setSpec sz RelAny
  (List (Var v2@(V _ (SetR r2) _)) expr) | Name v1 == Name v2 -> do
    Refl <- sameRep r r2
    xs <- mapM (simplifyAtType r2) expr
    setSpec (SzExact (length xs)) (relEqual r (makeFromList xs))
  (Var v2 :=: expr) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec (SzExact (Set.size set)) (relEqual r set)
  (Var v2@(V _ (SetR r2) _) :<-: target) | (Name v1 == Name v2) -> do
    Refl <- sameRep r r2
    x <- simplifyTarget @era @(Set a) target
    setSpec (SzExact (Set.size x)) (relEqual r x)
  (expr :=: v2@(Var _)) -> solveSet v1 (v2 :=: expr)
  (expr1 :=: Restrict (Var v2@(V _ (SetR a) _)) expr2) | Name v1 == Name v2 -> do
    Refl <- sameRep a r
    val1 <- simplify expr1
    val2 <- simplify expr2
    setSpec (SzExact (Map.size val1)) (relEqual r (Map.keysSet val2))
  (Restrict expr1 expr2 :=: expr3) -> solveSet v1 (expr3 :=: Restrict expr1 expr2)
  (Var v2 `Subset` expr) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec (SzMost (Set.size set)) (relSubset r set)
  (expr `Subset` Var v2) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec (SzLeast (Set.size set)) (relSuperset r set)
  (Disjoint (Var v2) expr) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec SzAny (relDisjoint r set)
  (Disjoint expr (Var v2)) -> solveSet v1 (Disjoint (Var v2) expr)
  (Member (Left (HashS expr)) (Var v2@(V _ (SetR ScriptHashR) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep r ScriptHashR
    ScriptF _ x <- simplifyAtType (ScriptR reify) expr
    let hash = hashScript @era x
    setSpec (SzLeast 1) (relSuperset r (Set.singleton hash))
  (Member (Right (HashS expr)) (Var v2@(V _ (SetR ScriptHashR) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep r ScriptHashR
    ScriptF _ x <- simplifyAtType (ScriptR reify) expr
    let hash = hashScript @era x
    setSpec (SzLeast 1) (relSuperset r (Set.singleton hash))
  (Member (Left (HashD expr)) (Var v2@(V _ (SetR DataHashR) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep r DataHashR
    x <- simplifyAtType DataR expr
    let hash = hashData @era x
    setSpec (SzLeast 1) (relSuperset r (Set.singleton hash))
  (Member (Right (HashD expr)) (Var v2@(V _ (SetR DataHashR) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep r DataHashR
    x <- simplifyAtType DataR expr
    let hash = hashData @era x
    setSpec (SzLeast 1) (relSuperset r (Set.singleton hash))
  (Member expr (Var v2@(V _ (SetR a) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep a r
    x <- simplify (direct expr)
    setSpec (SzLeast 1) (relSuperset r (Set.singleton x))
  (NotMember expr (Var v2@(V _ (SetR a) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep a r
    x <- simplify expr
    setSpec SzAny (relDisjoint r (Set.singleton x))
  (List (Var v2@(V _ (SetR r2) _)) xs) | Name v1 == Name v2 -> do
    Refl <- sameRep r r2
    ys <- mapM simplify xs
    pure $ SetSpec (SzMost (length ys)) (relEqual r (makeFromList ys))
  (Random (Var v2)) | Name v1 == Name v2 -> setSpec SzAny RelAny
  (ProjS lensbt trep (Var v2@(V _ (SetR brep) _)) :=: Dom (Lit (MapR drep _) x))
    | Name v1 == Name v2 -> do
        Refl <- sameRep r brep
        setSpec (SzExact (Map.size x)) (RelLens lensbt r trep (relEqual drep (Map.keysSet x)))
  (ProjS lensbt trep (Var v2@(V _ (SetR brep) _)) `Subset` Dom (Lit (MapR drep _) x))
    | Name v1 == Name v2 -> do
        Refl <- sameRep r brep
        setSpec (SzMost (Map.size x)) (RelLens lensbt r trep (relSubset drep (Map.keysSet x)))
  cond -> failT ["Can't solveSet " ++ show cond ++ " for variable " ++ show v1]

solveSets :: V era (Set a) -> [Pred era] -> Typed (SetSpec era a)
solveSets v@(V nm (SetR _) _) cs =
  explain ("\nSolving for " ++ nm ++ ", Set Predicates\n" ++ unlines (map (("  " ++) . show) cs)) $
    foldlM' accum mempty cs
  where
    accum spec cond = do
      condspec <- solveSet v cond
      liftT (spec <> condspec)

-- ========================================================
-- Solving for variables with an Adds instance

solveSum :: Adds t => V era t -> Pred era -> Typed (AddsSpec t)
solveSum v1@(V nam r _) predx =
  case predx of
    (Sized expr (Var v2@(V nm _ _))) | Name v1 == Name v2 -> do
      sz <- simplify expr
      pure $ AddsSpecSize nm sz
    (Sized (Var v2) expr) | Name v1 == Name v2 -> do
      n <- simplifyAtType r expr
      pure $ varOnLeft nam EQL n
    (expr :=: (Var v2)) | Name v1 == Name v2 -> do
      n <- simplifyAtType r expr
      pure $ varOnLeft nam EQL n
    ((Var v2) :=: expr) | Name v1 == Name v2 -> do
      n <- simplifyAtType r expr
      pure $ varOnLeft nam EQL n
    (expr :=: Delta (Var v2@(V _ CoinR _))) | Name v1 == Name v2 -> do
      DeltaCoin n <- simplify expr
      pure $ varOnLeft nam EQL (Coin n)
    -- This is an EQL test (x :=: y), so whether the
    -- variable is on the Left or the Right does not matter
    (Delta (Var v2) :=: expr) -> solveSum v1 (expr :=: (Delta (Var v2)))
    (expr :=: Negate (Var v2)) | Name v1 == Name v2 -> do
      Refl <- sameRep r DeltaCoinR
      DeltaCoin n <- simplifyAtType DeltaCoinR expr
      pure $ varOnLeft nam EQL (DeltaCoin (-n))
    (Negate (Var v2) :=: expr) | Name v1 == Name v2 -> do
      Refl <- sameRep r DeltaCoinR
      DeltaCoin n <- simplifyAtType DeltaCoinR expr
      pure $ varOnLeft nam EQL (DeltaCoin (-n))
    (Random (Var v2)) | Name v1 == Name v2 -> pure AddsSpecAny
    xx@(SumsTo x (Delta (Lit _ n)) cond xs@(_ : _)) -> do
      (rhsTotal, needsNeg) <- intSumWithUniqueV v1 xs
      case trace ("\nHERE "++show xx++"\nrhsTotal "++show rhsTotal++"\n NEEDSNEG "++show needsNeg) r of
        CoinR ->
          if needsNeg
            then pure (varOnRightNeg n cond (fromI ["solveSum-SumsTo 1"] rhsTotal) nam)
            else
              if rhsTotal < 0
                then 
                  pure (varOnRight (add n (Coin $ fromIntegral $ negate rhsTotal)) cond (Coin 0) nam)
                else
                  pure (varOnRight n cond (fromI ["solveSum-SumsTo 2", show n, show rhsTotal, show x, show v1] rhsTotal) nam)
        DeltaCoinR ->
          if needsNeg
            then pure (varOnRightNeg n cond (fromI ["solveSum-SumsTo 3"] rhsTotal) nam)
            else pure (varOnRight n cond (fromI ["solveSum-SumsTo 4"] rhsTotal) nam)
        other -> failT [show predx, show other ++ " should be either Coin or DeltaCoin"]
    (SumsTo _ (Lit r2 n) cond xs@(_ : _)) -> do
      (rhsTotal, needsNeg) <- intSumWithUniqueV v1 xs
      Refl <- sameRep r r2
      if needsNeg
        then pure (varOnRightNeg n cond (fromI ["solveSum-SumsTo 5"] rhsTotal) nam)
        else pure (varOnRight n cond (fromI ["solveSum-SumsTo 6"] rhsTotal) nam)
    (SumsTo _ (Var v2@(V _ r2 _)) cond xs@(_ : _)) | Name v1 == Name v2 -> do
      rhsTotal <- summandsAsInt xs
      Refl <- sameRep r r2
      pure $ varOnLeft nam cond rhsTotal
    (SumsTo _ (Delta (Var v2)) cond xs@(_ : _)) | Name v1 == Name v2 -> do
      rhsTotal <- summandsAsInt xs
      case r of
        CoinR -> pure $ varOnLeft nam cond rhsTotal
        DeltaCoinR -> pure $ varOnLeft nam cond rhsTotal
        other -> failT [show predx, show other ++ " should be either Coin or DeltaCoin"]
    (Var v2@(V nm r2 _) :<-: tar) | Name v1 == Name v2 -> do
      Refl <- sameRep r r2
      x <- simplifyTarget @era @t tar
      pure (AddsSpecSize nm (SzExact (toI x)))
    other -> failT ["Can't solveSum " ++ show (Name v1) ++ " = " ++ show other]

solveSums :: Adds t => V era t -> [Pred era] -> Typed (AddsSpec t)
solveSums v@(V nm r _) cs =
  explain ("\nGiven (Add " ++ show r ++ "), Solving for " ++ nm ++ " :: " ++ show r ++ ",  with Predicates \n" ++ unlines (map (("  " ++) . show) cs)) $
    foldlM' accum mempty cs
  where
    accum spec cond = do
      sumVspec <- solveSum v cond
      explain
        ("Solving Sum constraint (" ++ show cond ++ ") for variable " ++ show nm)
        (liftT (spec <> sumVspec))

summandAsInt :: Adds c => Sum era c -> Typed Int
summandAsInt (One (Lit _ x)) = pure (toI x)
summandAsInt (One (Delta (Lit CoinR (Coin n)))) = pure (toI (DeltaCoin n))
summandAsInt (One (Negate (Lit DeltaCoinR (DeltaCoin n)))) = pure (toI ((DeltaCoin (-n))))
summandAsInt (ProjOne l CoinR (Lit _ x)) = pure (toI (x ^. l))
summandAsInt (SumMap (Lit _ m)) = pure (toI (Map.foldl' add zero m))
summandAsInt (SumList (Lit _ m)) = pure (toI (List.foldl' add zero m))
summandAsInt (ProjMap _ l (Lit _ m)) = pure (toI (List.foldl' (\ans x -> add ans (x ^. l)) zero m))
summandAsInt x = failT ["Can't compute summandAsInt: " ++ show x ++ ", to an Int."]

genSum :: Adds x => Sum era x -> Rep era x -> x -> Subst era -> Gen (Subst era)
genSum (One (Var v)) rep x sub = pure $ extend v (Lit rep x) sub
genSum (One (Delta (Var v))) DeltaCoinR d sub = pure $ extend v (Lit CoinR (fromI [] (toI d))) sub
genSum (One (Negate (Var v))) DeltaCoinR (DeltaCoin n) sub = pure $ extend v (Lit DeltaCoinR (DeltaCoin (-n))) sub
genSum other rep x _ = errorMess ("Can't genSum " ++ show other) [show rep, show x]

summandsAsInt :: Adds c => [Sum era c] -> Typed Int
summandsAsInt [] = pure 0
summandsAsInt (x : xs) = do
  n <- summandAsInt x
  m <- summandsAsInt xs
  pure (m + n)

sameV :: V era s -> V era t -> Typed (s :~: t)
sameV (V _ r1 _) (V _ r2 _) = sameRep r1 r2

unique2 :: Adds c => V era t -> (Int, Bool, [Name era]) -> Sum era c -> Typed (Int, Bool, [Name era])
unique2 v1 (c, b, ns) (One (Var v2)) =
  if Name v1 == Name v2
    then pure (c, b, Name v2 : ns)
    else failT ["Unexpected Name in 'unique' " ++ show v2]
unique2 v1 (c, b, ns) (One (Delta (Var v2@(V _ CoinR _)))) =
  if Name v1 == Name v2
    then pure (c, b, Name v1 : ns)
    else failT ["Unexpected Name in 'unique' " ++ show v2]
unique2 v1 (c, _, ns) (One (Negate (Var v2@(V _nam DeltaCoinR _)))) =
  if Name v1 == Name v2
    then pure (c, True, Name v2 : ns)
    else failT ["Unexpected Name in 'unique' " ++ show v2]
unique2 v1 (c, b, ns) (ProjOne _ _ (Var v2)) =
  if Name v1 == Name v2
    then pure (c, b, Name v2 : ns)
    else failT ["Unexpected Name in 'unique' " ++ show v2]
unique2 _ (c1, b, ns) sumexpr = do c2 <- summandAsInt sumexpr; pure (c1 + c2, b, ns)

-- | Check that there is exactly 1 occurence of 'v',
--   and return the sum of the other terms in 'ss'
--   which should all be constants.
intSumWithUniqueV :: Adds c => V era t -> [Sum era c] -> Typed (Int, Bool)
intSumWithUniqueV v@(V nam _ _) ss = do
  (c, b, ns) <- foldlM' (unique2 v) (0, False, []) ss
  case ns of
    [_] -> pure (c, b)
    [] -> failT ["Failed to find the unique name: " ++ nam ++ " in" ++ show ss]
    (_ : _ : _) -> failT ["The expected unique name: " ++ nam ++ " occurs more than once in " ++ show ss]

-- ===========================================================
-- Solving for variables with type List

-- | Given a variable: 'v1', with a List type, compute a ListSpec
--   which describes the constraints implied by the Pred 'predicate'
solveList :: V era [a] -> Pred era -> Typed (ListSpec era a)
solveList v1@(V _ (ListR r) _) predicate = case predicate of
  (Sized (Size sz) (Var v2)) | Name v1 == Name v2 -> pure $ ListSpec sz ElemAny
  (Var v2 :=: expr) | Name v1 == Name v2 -> do
    With xs <- simplifyList r expr
    pure $ ListSpec (SzExact (length xs)) (ElemEqual r xs)
  (expr :=: v2@(Var _)) -> solveList v1 (v2 :=: expr)
  (Random (Var v2)) | Name v1 == Name v2 -> pure $ ListSpec SzAny ElemAny
  (List (Var v2@(V _ r2 _)) xs) | Name v1 == Name v2 -> do
    let r3 = tsRep r2
    Refl <- sameRep r r3
    ys <- mapM simplify xs
    pure $ ListSpec (SzExact (length ys)) (ElemEqual r ys)
  cond -> failT ["Can't solveList " ++ show cond ++ " for variable " ++ show v1]

solveLists :: V era [a] -> [Pred era] -> Typed (ListSpec era a)
solveLists v@(V nm (ListR _) _) cs =
  explain ("\nSolving for " ++ nm ++ ", List Predicates\n" ++ unlines (map (("  " ++) . show) cs)) $
    foldlM' accum mempty cs
  where
    accum spec cond = do
      condspec <- solveList v cond
      liftT (spec <> condspec)

-- ===================================================
-- Helper functions for use in 'dispatch'

-- | Combine solving an generating for a variable with a 'Counts' instance
genCount :: (Count t, Era era) => V era t -> [Pred era] -> Typed (Gen t)
genCount v1@(V _ rep _) [Random (Var v2)] | Name v1 == Name v2 = pure (genRep rep)
genCount v1@(V _ r1 _) [Var v2@(V _ r2 _) :=: expr] | Name v1 == Name v2 = do
  Refl <- sameRep r1 r2
  val <- simplify expr
  pure (pure val)
genCount v1@(V _ r1 _) [expr :=: Var v2@(V _ r2 _)] | Name v1 == Name v2 = do
  Refl <- sameRep r1 r2
  val <- simplify expr
  pure (pure val)
genCount v1@(V _ r1 _) [CanFollow succExpr (Var v2@(V _ r2 _))] | Name v1 == Name v2 = do
  Refl <- sameRep r1 r2
  succVal <- simplify succExpr
  pure (genPred succVal)
genCount v1@(V _ r1 _) [CanFollow (Var v2@(V _ r2 _)) predExpr] | Name v1 == Name v2 = do
  Refl <- sameRep r1 r2
  predVal <- simplify predExpr
  pure (genSucc predVal)
genCount v1@(V _ r1 _) [Sized sz (Var v2@(V _ EpochR _))] | Name v1 == Name v2 = do
  Refl <- sameRep r1 EpochR
  size <- simplify sz
  pure $ do
    n <- genFromIntRange size
    pure $ EpochNo $ fromIntegral n
genCount v1@(V _ r1 _) [Sized sz (Var v2@(V _ SlotNoR _))] | Name v1 == Name v2 = do
  Refl <- sameRep r1 SlotNoR
  size <- simplify sz
  pure $ do
    n <- genFromIntRange size
    pure $ SlotNo $ fromIntegral n
genCount v@(V _ r _) cs = failT zs
  where
    zs = ("Cannot solve: genCount " ++ show v ++ " at type " ++ show r ++ " on Predicates") : map show cs

-- | Used in solving Projections
data Update t where
  Update :: s -> Lens' t s -> Update t

update :: t -> [Update t] -> t
update t [] = t
update t (Update s l : more) = update (Lens.set l s t) more

anyToUpdate :: Rep era t1 -> (AnyF era t2) -> Typed (Update t1)
anyToUpdate rep1 (AnyF (FConst _ s rep2 l)) = do
  Refl <- sameRep rep1 rep2
  pure (Update s l)
anyToUpdate _ x = failT ["component is not FConst: " ++ show x]

intToNatural :: Int -> Natural
intToNatural n = fromIntegral n

isIf :: Pred era -> Bool
isIf (If _ _ _) = True
isIf _ = False

-- ==================================================================================
-- Given a variable ('v1' :: 't') and [Pred] that constrain it. Produce a (Gen t)

-- | Dispatch on the type of the variable 'v1' being solved.
dispatch :: forall t era. Era era => V era t -> [Pred era] -> Typed (Gen t)
dispatch v1 preds | Just (If tar x y) <- List.find isIf preds = do
  b <- simplifyTarget tar
  let others = filter (not . isIf) preds
  if b then dispatch v1 (x : others) else dispatch v1 (y : others)
dispatch v1@(V nam r1 _) preds = explain ("Solving for variable " ++ nam ++ show preds) $ case preds of
  [Var v2 :=: Lit r2 t] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    pure (pure t)
  [Lit r2 t :=: Var v2] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    pure (pure t)
  [Sized (Var v2@(V _ r2 _)) term] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    x <- simplify term
    pure $ pure (SzExact (getSize x))
  [Sized sizeterm (Var v2@(V _ r2 _))] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    sz <- simplify sizeterm
    pure $ do n <- genFromSize sz; genSizedRep n r2
  [cc@(Component (Right (Var v2)) cs)] | Name v1 == Name v2 -> explain ("Solving " ++ show cc) $ do
    pairs <- mapM (anyToUpdate r1) cs
    pure $ do t <- genRep r1; pure (update t pairs)
  [Component (Right (Var v2)) cs, Random (Var v3)] | Name v1 == Name v2 && Name v1 == Name v3 -> do
    pairs <- mapM (anyToUpdate r1) cs
    pure $ do
      t <- genRep r1
      pure (update t pairs)
  [Random (Var v3), Component (Right (Var v2)) cs] | Name v1 == Name v2 && Name v1 == Name v3 -> do
    pairs <- mapM (anyToUpdate r1) cs
    pure $ do
      t <- genRep r1
      pure (update t pairs)
  [Sized (Var v2) (Lit SizeR x)] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 SizeR
    pure (pure (SzExact (getSize x)))
  [Sized (Lit SizeR sz) (Var v2)] | isAddsType r1 && Name v1 == Name v2 -> do
    With _ <- hasAdds r1 r1
    pure $ fromI ["dispatch " ++ show v1 ++ " " ++ show preds] <$> genFromIntRange sz
  [GenFrom (Var v2@(V _ r2 _)) target] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    x <- simplifyTarget @era @(Gen t) target
    pure x
  [Var v2@(V _ r2 _) :<-: target] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    x <- simplifyTarget @era @t target
    pure (pure x)
  [pred1@(Member (Left (HashS (Var v2@(V _ (ScriptR p) _)))) (Dom expr))] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 (ScriptR p)
    case termRep expr of
      mt@(MapR ScriptHashR (ScriptR _)) -> do
        m <- simplifyAtType mt expr
        pure (snd <$> genFromMap ["dispatch " ++ show v1 ++ " " ++ show preds] m)
      other -> failT ["The Pred: " ++ show pred1, "Can only be applied to a map whose range is 'Script'.", show other]
  [pred1@(Member (Right (HashS (Var v2@(V _ (ScriptR p) _)))) (Dom expr))] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 (ScriptR p)
    case termRep expr of
      mt@(MapR ScriptHashR (ScriptR _)) -> do
        m <- simplifyAtType mt expr
        pure (snd <$> genFromMap ["dispatch " ++ show v1 ++ " " ++ show preds] m)
      other -> failT ["The Pred: " ++ show pred1, "Can only be applied to a map whose range is 'Script'.", show other]
  [pred1@(Member (Left (HashD (Var v2@(V _ DataR _)))) (Dom expr))] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 DataR
    case termRep expr of
      mt@(MapR DataHashR DataR) -> do
        m <- simplifyAtType mt expr
        pure (snd <$> genFromMap ["dispatch " ++ show v1 ++ " " ++ show preds] m)
      other -> failT ["The Pred: " ++ show pred1, "Can only be applied to a map whose range is 'Data'.", show other]
  [pred1@(Member (Right (HashD (Var v2@(V _ DataR _)))) (Dom expr))] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 DataR
    case termRep expr of
      mt@(MapR DataHashR DataR) -> do
        m <- simplifyAtType mt expr
        pure (snd <$> genFromMap ["dispatch " ++ show v1 ++ " " ++ show preds] m)
      other -> failT ["The Pred: " ++ show pred1, "Can only be applied to a map whose range is 'Data'.", show other]
  [Member (Left (Var v2@(V _ r2 _))) expr] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    set <- simplify expr
    let msgs = ("Solving for variable " ++ nam) : map show preds
    if Set.null set
      then failT (("The set is empty " ++ synopsis (termRep expr) set ++ ", can't find an element.") : msgs)
      else pure (fst <$> itemFromSet msgs set)
  [Member (Right (Var v2@(V _ r2 _))) expr] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    set <- simplify expr
    let msgs = ("Solving for variable " ++ nam) : map show preds
    if Set.null set
      then failT (("The set is empty " ++ synopsis (termRep expr) set ++ ", can't find an element.") : msgs)
      else pure (fst <$> itemFromSet msgs set)
  [NotMember (Var v2@(V _ r2 _)) expr] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    set <- simplify expr
    let msgs = ("Solving for variable " ++ nam) : map show preds
    pure $ suchThatErr msgs (genRep r2) (`Set.notMember` set)
  [MapMember (Var v2@(V _ r2 _)) exprVal exprMap] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    m <- simplify (direct exprMap)
    v <- simplify exprVal
    let m2 = Map.filter (== v) m
    let msgs = ("Solving for variable " ++ nam) : map show preds
    if Map.null m2
      then failT (("The value: " ++ synopsis (termRep exprVal) v ++ " is not in the range of the map.") : msgs)
      else pure (fst <$> itemFromSet msgs (Map.keysSet m2))
  [MapMember exprKey (Var v2@(V _ r2 _)) exprMap] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    m <- simplify (direct exprMap)
    k <- simplify exprKey
    let msgs = ("Solving for variable " ++ nam) : map show preds
    case Map.lookup k m of
      Just v -> pure (pure v)
      Nothing -> failT (("The key: " ++ synopsis (termRep exprKey) k ++ " is not in the map.") : msgs)
  [List (Var v2@(V _ (MaybeR r2) _)) expr] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 (MaybeR r2)
    xs <- mapM simplify expr
    pure $ pure (makeFromList xs)
  [Random (Var v2)] | Name v1 == Name v2 -> pure $ genRep r1
  cs -> case r1 of
    MapR dom rng -> do
      spec <- solveMaps v1 cs
      pure $ genFromMapSpec nam (map show cs) (genRep dom) (genRep rng) spec
    SetR r -> do
      spec <- solveSets v1 cs
      pure $ genFromSetSpec [] (genRep r) spec
    ListR r -> do
      spec <- solveLists v1 cs
      pure $ genFromListSpec [] (genRep r) spec
    _other
      | isAddsType r1 -> do
          With v2 <- hasAdds r1 v1
          sumv <- solveSums v2 cs
          let msgs = ("Solving for variable " ++ nam) : map show preds
          pure $ genAdds msgs sumv
      | isCountType r1 -> do
          With v2 <- hasCount r1 v1
          genCount v2 cs
      | otherwise ->
          failT
            [ "No solution for "
                ++ nam
                ++ " at type "
                ++ show r1
                ++ " for conditions "
                ++ show cs
                ++ show (List.nubBy cpeq cs)
            ]

genOrFail ::
  Era era =>
  Bool ->
  Either [String] (Subst era) ->
  ([Name era], [Pred era]) ->
  Gen (Either [String] (Subst era))
genOrFail loud (Right subst) ([Name v@(V _ rep _)], conds) =
  case runTyped $
    ifTrace
      loud
      (pad 20 (show (Name v)) ++ " | " ++ showL show "," (map (substPred subst) conds))
      (dispatch v (map (substPred subst) conds)) of
    Right gen -> do
      t <- gen
      ifTrace
        loud
        ("   " ++ synopsis rep t)
        (pure (Right (extend v (Lit rep t) subst)))
    Left msgs -> pure (Left msgs)
genOrFail _ (Right _) (names, _) = error ("Multiple names not handed yet " ++ show names)
genOrFail _ (Left msgs) _ = pure (Left msgs)

genOrFailList ::
  Era era =>
  Bool ->
  Either [String] (Subst era) ->
  [([Name era], [Pred era])] ->
  Gen (Either [String] (Subst era))
genOrFailList loud = foldlM' (genOrFail loud)

genDependGraph :: Bool -> Proof era -> DependGraph era -> Gen (Either [String] (Subst era))
genDependGraph loud (Shelley _) (DependGraph pairs) = genOrFailList loud (Right emptySubst) pairs
genDependGraph loud (Allegra _) (DependGraph pairs) = genOrFailList loud (Right emptySubst) pairs
genDependGraph loud (Mary _) (DependGraph pairs) = genOrFailList loud (Right emptySubst) pairs
genDependGraph loud (Alonzo _) (DependGraph pairs) = genOrFailList loud (Right emptySubst) pairs
genDependGraph loud (Babbage _) (DependGraph pairs) = genOrFailList loud (Right emptySubst) pairs
genDependGraph loud (Conway _) (DependGraph pairs) = genOrFailList loud (Right emptySubst) pairs

-- | Solve for one variable, and add its solution to the substitution
solveOneVar :: Era era => Subst era -> ([Name era], [Pred era]) -> Gen (Subst era)
solveOneVar subst ([Name (v@(V _ r _))], ps) = do
  !genOneT <- monadTyped (dispatch v (map (substPred subst) ps)) -- Sub solution for previously solved variables
  !t <- genOneT
  pure (extend v (Lit r t) subst)
solveOneVar subst0 (names, preds) = case (names, map (substPred subst0) preds) of
  (ns, [SumSplit small tot EQL suml]) | length ns == length suml -> do
    !n <- monadTyped $ simplify tot
    !zs <- partition small ["Partition, while solving multiple SumsTo vars.", show ns, show preds] (length ns) n
    let rep = termRep tot
    foldlM' (\ !sub (!sumx, !x) -> genSum (substSum sub sumx) rep x sub) subst0 (zip suml zs)
  (ns, ps) -> errorMess "Not yet. multiple vars in solveOneVar" [show ns, show ps]

toolChainSub :: Era era => Proof era -> OrderInfo -> [Pred era] -> Subst era -> Gen (Subst era)
toolChainSub _proof order cs subst0 = do
  (_count, DependGraph pairs) <- compileGen order (map (substPredWithVarTest subst0) cs)
  Subst subst <- foldlM' solveOneVar subst0 pairs
  let isTempV k = not (elem '.' k)
  pure $ (Subst (Map.filterWithKey (\k _ -> isTempV k) subst))

toolChain :: Era era => Proof era -> OrderInfo -> [Pred era] -> Subst era -> Gen (Env era)
toolChain _proof order cs subst0 = do
  (_count, DependGraph pairs) <- compileGen order (map (substPredWithVarTest subst0) cs)
  subst <- foldlM' solveOneVar subst0 pairs
  monadTyped $ substToEnv subst emptyEnv

