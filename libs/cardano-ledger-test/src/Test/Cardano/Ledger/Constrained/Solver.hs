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

import Cardano.Ledger.BaseTypes (EpochNo (EpochNo), SlotNo (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Core (Era (..))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (foldlM')
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Lens.Micro (Lens')
import qualified Lens.Micro as Lens
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (
  Adds (..),
  Count (..),
  FromList (..),
  Sizeable (getSize),
  Sums (..),
 )
import Test.Cardano.Ledger.Constrained.Combinators (itemFromSet, suchThatErr)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Rewrite (DependGraph (..), OrderInfo, compileGen)
import Test.Cardano.Ledger.Constrained.Size (
  AddsSpec (..),
  OrdCond (..),
  Size (..),
  genFromIntRange,
  vLeft,
  vRight,
  vRightNeg,
  vRightSize,
 )
import Test.Cardano.Ledger.Constrained.Spec (
  ElemSpec (..),
  ListSpec (..),
  MapSpec (..),
  RelSpec (..),
  RngSpec (..),
  SetSpec (..),
  Unique (..),
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
  synopsis,
  testEql,
  (:~:) (Refl),
 )
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (Proof (..))
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

-- =================

hasOrd :: Rep era t -> s t -> Typed (HasConstraint Ord (s t))
hasOrd rep xx = explain ("'hasOrd " ++ show rep ++ "' fails") (help rep xx)
  where
    err t c = error ("hasOrd function 'help' evaluates its second arg at type " ++ show t ++ ", in " ++ c ++ " case.")
    help :: Rep era t -> s t -> Typed (HasConstraint Ord (s t))
    help CoinR t = pure $ With t
    help r@(_ :-> _) _ = failT [show r ++ " does not have an Ord instance."]
    help r@(MapR _ b) m = do
      With _ <- help b (err b (show r))
      pure (With m)
    help (SetR _) s = pure $ With s
    help r@(ListR a) l = do
      With _ <- help a (err a (show r))
      pure $ With l
    help CredR c = pure $ With c
    help PoolHashR p = pure $ With p
    help GenHashR p = pure $ With p
    help GenDelegHashR p = pure $ With p
    help WitHashR p = pure $ With p
    help PoolParamsR pp = pure $ With pp
    help EpochR e = pure $ With e
    help RationalR r = pure $ With r
    help Word64R w = pure $ With w
    help IntR i = pure $ With i
    help NaturalR i = pure $ With i
    help FloatR i = pure $ With i
    help TxInR t = pure $ With t
    help CharR s = pure $ With s
    help (ValueR (Shelley _)) v = pure $ With v
    help (ValueR (Allegra _)) v = pure $ With v
    help UnitR v = pure $ With v
    help (PairR a b) p = do
      With _ <- help a undefined
      With _ <- help b undefined
      pure $ With p
    help (ValueR _) _ = failT ["Value does not have Ord instance in post Allegra eras"]
    help (TxOutR _) _ = failT ["TxOut does not have Ord instance"]
    help (UTxOR _) _ = failT ["UTxO does not have Ord instance"]
    help DeltaCoinR v = pure $ With v
    help GenDelegPairR v = pure $ With v
    help FutureGenDelegR v = pure $ With v
    help (PPUPStateR _) _ = failT ["PPUPState does not have Ord instance"]
    help PtrR v = pure $ With v
    help SnapShotsR _ = failT ["SnapShot does not have Ord instance"]
    help IPoolStakeR _ = failT ["IndividualPoolStake does not have Ord instance"]
    help (PParamsR _) _ = failT ["PParams does not have Ord instance"]
    help (PParamsUpdateR _) _ = failT ["PParamsUpdate does not have Ord instance"]
    help RewardR v = pure $ With v
    help r@(MaybeR a) l = do
      With _ <- help a (err a (show r))
      pure $ With l
    help NewEpochStateR _ = failT ["NewEpochStateR does not have Ord instance"]
    help (ProtVerR _) v = pure $ With v
    help SlotNoR v = pure $ With v
    help SizeR v = pure $ With v
    help VCredR v = pure $ With v
    help VHashR v = pure $ With v
    help MultiAssetR _ = failT ["MultiAsset does not have Ord instance"]
    help PolicyIDR v = pure $ With v
    help (WitnessesFieldR _) _ = failT ["WitnessesField does not have Ord instance"]
    help AssetNameR v = pure $ With v
    help DCertR _ = failT ["DCert does not have Ord instance"]
    help RewardAcntR v = pure $ With v
    help ValidityIntervalR v = pure $ With v

hasEq :: Rep era t -> s t -> Typed (HasConstraint Eq (s t))
hasEq rep xx = explain ("'hasOrd " ++ show rep ++ "' fails") (help rep xx)
  where
    help :: Rep era t -> s t -> Typed (HasConstraint Eq (s t))
    help (TxOutR _) v = pure $ With v
    help x v = do
      With y <- hasOrd x v
      pure (With y)

-- ===============================

-- | Is there an Adds instance for 't'
hasAdds :: Rep era t -> (s t) -> Typed (HasConstraint Adds (s t))
hasAdds IntR x = pure $ With x
hasAdds Word64R x = pure $ With x
hasAdds CoinR x = pure $ With x
hasAdds DeltaCoinR x = pure $ With x
hasAdds RationalR x = pure $ With x
hasAdds NaturalR x = pure $ With x
hasAdds r _ = failT [show r ++ " does not have Adds instance."]

isAddsType :: forall era t. Rep era t -> Bool
isAddsType rep = case hasAdds rep rep of
  (Typed (Right (With _))) -> True
  (Typed (Left _)) -> False

-- ==================================

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

simplifyList :: Rep era rng -> Term era y -> Typed (HasConstraint Ord [rng])
simplifyList r1 term = do
  x <- simplify term
  Refl <- sameRep (ListR r1) (termRep term)
  hasOrd r1 x

-- | Is the Sum a variable (of a map). Only SumMap and Project store maps.
isMapVar :: Name era -> Sum era c -> Bool
isMapVar n1 (SumMap (Var v2)) = n1 == Name v2
isMapVar n1 (Project _ (Var v2)) = n1 == Name v2
isMapVar _ _ = False

exactlyOne :: (a -> Bool) -> [a] -> Bool
exactlyOne pp xs = 1 == length (filter pp xs)

-- | Make a generator for a Map type when there is a Projection from the domain of the map.
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
    | Name v1 == Name v2 -> mapSpec sz RelAny RngAny
  (Sized (Lit SizeR sz) (Dom (Var v2)))
    | Name v1 == Name v2 -> mapSpec sz RelAny RngAny
  (Var v2 :=: expr) | Name v1 == Name v2 -> do
    m1 <- simplifyAtType r expr
    With _ <- hasEq rng rng
    mapSpec SzAny (relEqual dom (Map.keysSet m1)) (RngElem rng (Map.elems m1))
  (expr :=: v2@(Var _)) -> solveMap v1 (v2 :=: expr)
  (expr1 :=: Restrict expr2 (Var v2@(V _ (MapR a _) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep a dom
    val1 <- simplify expr1
    val2 <- simplify expr2
    mapSpec (SzLeast (Map.size val1)) (relSuperset dom val2) RngAny
  (Restrict expr1 expr2 :=: expr3) -> solveMap v1 (expr3 :=: Restrict expr1 expr2)
  -- TODO recast these in terms of Fields
  (ProjS lensbt _trep (Dom (Var v2@(V _ (MapR brep _) _))) :=: Lit (SetR _srep) x) | Name v1 == Name v2 -> do
    Refl <- sameRep dom brep
    pure (MapUnique (MapR dom rng) (Unique (show predicate) (projOnDom x lensbt dom rng)))
  (ProjS lensbt _trep (Dom (Var v2@(V _ (MapR brep _) _))) :=: Dom (Lit (MapR _drep _) x))
    | Name v1 == Name v2 -> do
        Refl <- sameRep dom brep
        pure (MapUnique (MapR dom rng) (Unique (show predicate) (projOnDom (Map.keysSet x) lensbt dom rng)))
  (Lit (SetR _srep) x) :=: ProjS lensbt _trep (Dom (Var v2@(V _ (MapR brep _) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom brep
    pure (MapUnique (MapR dom rng) (Unique (show predicate) (projOnDom x lensbt dom rng)))
  (expr :=: Rng (Var v2))
    | Name v1 == Name v2 -> do
        s <- simplify expr
        let SetR a = termRep expr
        Refl <- sameRep rng a
        mapSpec (SzLeast (Set.size s)) RelAny (RngRel (relEqual rng s))
  (Rng (Var v2) :=: expr)
    | Name v1 == Name v2 -> do
        s <- simplify expr
        let SetR a = termRep expr
        Refl <- sameRep rng a
        mapSpec (SzLeast (Set.size s)) RelAny (RngRel (relEqual rng s))
  (Rng (Var v2) `Subset` expr)
    | Name v1 == Name v2 -> do
        With _ <- hasOrd rng rng
        With n <- simplifySet rng expr
        mapSpec SzAny RelAny (RngRel (relSubset rng n))
  (expr `Subset` Rng (Var v2))
    | Name v1 == Name v2 -> do
        With _ <- hasOrd rng rng
        With n <- simplifySet rng expr
        mapSpec SzAny RelAny (RngRel (relSuperset rng n))
  (Rng expr `Subset` (Var v2))
    | Name v1 == Name v2 -> do
        With _ <- hasOrd rng rng
        With n <- simplifySet rng expr
        mapSpec SzAny RelAny (RngRel (relSuperset rng n))
  (expr :=: Dom (Var v2))
    | Name v1 == Name v2 -> do
        let SetR a = termRep expr
        Refl <- sameRep dom a
        mm <- simplify expr
        mapSpec (SzExact (Set.size mm)) (relSubset dom mm) RngAny
  (Dom (Var v2) :=: expr)
    | Name v1 == Name v2 -> do
        let SetR a = termRep expr
        Refl <- sameRep dom a
        mm <- simplify expr
        mapSpec (SzExact (Set.size mm)) (relSubset dom mm) RngAny
  (Dom (Var v2) `Subset` expr)
    | Name v1 == Name v2 -> do
        With _ <- hasOrd dom dom
        With n <- simplifySet dom expr
        mapSpec (SzMost (Set.size n)) (relSubset dom n) RngAny
  (expr `Subset` Dom (Var v2))
    | Name v1 == Name v2 -> do
        With _ <- hasOrd dom dom
        With n <- simplifySet dom expr
        mapSpec (SzLeast (Set.size n)) (relSuperset dom n) RngAny
  (SumsTo small expr cond xs) | exactlyOne (isMapVar (Name v1)) xs -> do
    t <- simplify expr
    rngspec <- solveMapSummands small t [msg] cond v1 0 xs
    mapSpec SzAny RelAny rngspec
  (Random (Var v2)) | Name v1 == Name v2 -> mapSpec SzAny RelAny RngAny
  (Sized (Size sz) (Var v2)) | Name v1 == Name v2 -> mapSpec sz RelAny RngAny
  (Disjoint expr (Dom (Var v2))) | Name v1 == Name v2 -> do
    With set <- simplifySet dom expr
    mapSpec SzAny (relDisjoint dom set) RngAny
  (Disjoint (Dom (Var v2)) expr) | Name v1 == Name v2 -> do
    With set <- simplifySet dom expr
    mapSpec SzAny (relDisjoint dom set) RngAny
  (Disjoint expr (Rng (Var v2))) | Name v1 == Name v2 -> do
    With _ <- hasOrd rng rng
    With set <- simplifySet rng expr
    mapSpec SzAny RelAny (RngRel $ relDisjoint rng set)
  (Disjoint (Rng (Var v2)) expr) | Name v1 == Name v2 -> do
    With _ <- hasOrd rng rng
    With set <- simplifySet rng expr
    mapSpec SzAny RelAny (RngRel $ relDisjoint rng set)
  (Member expr (Dom (Var v2@(V _ (MapR a _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom a
    x <- simplify expr
    mapSpec (SzLeast 1) (relSuperset dom (Set.singleton x)) RngAny
  (Member expr (Rng (Var v2@(V _ (MapR _ b) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep rng b
    x <- simplify expr
    mapSpec (SzLeast 1) RelAny (RngRel (relSuperset rng (Set.singleton x)))
  (NotMember expr (Dom (Var v2@(V _ (MapR a _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom a
    x <- simplify expr
    mapSpec SzAny (relDisjoint dom (Set.singleton x)) RngAny
  (NotMember expr (Rng (Var v2@(V _ (MapR _ b) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep rng b
    x <- simplify expr
    mapSpec SzAny RelAny (RngRel (relDisjoint rng (Set.singleton x)))
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
  Int ->
  [Sum era c] ->
  Typed (RngSpec era rng)
solveMapSummands small lhsC _ cond (V _ (MapR _ r) _) c [Project crep (Var (V name (MapR _ r1) _))] = do
  Refl <- sameRep r r1
  pure (RngProj small crep (vRightSize (toI lhsC) cond c name))
solveMapSummands small lhsC _ cond (V _ (MapR _ r) _) c [SumMap (Var (V name (MapR _ r1) _))] = do
  Refl <- sameRep r r1
  pure (RngSum small (vRightSize (toI lhsC) cond c name))
solveMapSummands small lhsC msg cond v c (s : ss)
  | isMapVar (Name v) s =
      solveMapSummands small lhsC msg cond v c (ss ++ [s])
solveMapSummands small lhsC msg cond v c (s : ss) = do
  d <- summandAsInt s
  solveMapSummands small lhsC msg cond v (c + d) ss
solveMapSummands _ _ msg _ v _ [] = failT (("Does not have exactly one summand with variable " ++ show (Name v)) : msg)

solveMaps :: (Era era, Ord dom) => V era (Map dom rng) -> [Pred era] -> Typed (MapSpec era dom rng)
solveMaps v@(V _ (MapR _ _) _) cs =
  foldlM' accum (MapSpec SzAny RelAny RngAny) cs
  where
    accum spec cond = do
      condspec <- solveMap v cond
      liftT (spec <> condspec)

-- ===========================================================
-- Solving for variables with type Set

-- | Given a variable: 'v1', with a Set type, compute a SetSpec
--   which describes the constraints implied by the Pred 'predicate'
solveSet :: V era (Set a) -> Pred era -> Typed (SetSpec era a)
solveSet v1@(V _ (SetR r) _) predicate = case predicate of
  (Sized (Size sz) (Var v2)) | Name v1 == Name v2 -> setSpec sz RelAny
  (Var v2 :=: expr) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec (SzExact (Set.size set)) (relEqual r set)
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
  (Member expr (Var v2@(V _ (SetR a) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep a r
    x <- simplify expr
    setSpec (SzLeast 1) (relSuperset r (Set.singleton x))
  (NotMember expr (Var v2@(V _ (SetR a) _))) | Name v1 == Name v2 -> do
    Refl <- sameRep a r
    x <- simplify expr
    setSpec SzAny (relDisjoint r (Set.singleton x))
  (Random (Var v2)) | Name v1 == Name v2 -> setSpec SzAny RelAny
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
      pure $ vLeft nam EQL (toI n)
    (expr :=: (Var v2)) | Name v1 == Name v2 -> do
      n <- simplifyAtType r expr
      pure $ vLeft nam EQL (toI n)
    ((Var v2) :=: expr) | Name v1 == Name v2 -> do
      n <- simplifyAtType r expr
      pure $ vLeft nam EQL (toI n)
    (expr :=: Delta (Var v2@(V _ CoinR _))) | Name v1 == Name v2 -> do
      DeltaCoin n <- simplify expr
      pure $ vLeft nam EQL (toI (Coin n))
    -- This is an EQL test (x :=: y), so whether the
    -- variable is on the Left or the Right does not matter
    (Delta (Var v2) :=: expr) -> solveSum v1 (expr :=: (Delta (Var v2)))
    (expr :=: Negate (Var v2)) | Name v1 == Name v2 -> do
      Refl <- sameRep r DeltaCoinR
      DeltaCoin n <- simplifyAtType DeltaCoinR expr
      pure $ vLeft nam EQL (toI (DeltaCoin (-n)))
    (Negate (Var v2) :=: expr) | Name v1 == Name v2 -> do
      Refl <- sameRep r DeltaCoinR
      DeltaCoin n <- simplifyAtType DeltaCoinR expr
      pure $ vLeft nam EQL (toI (DeltaCoin (-n)))
    (Random (Var v2)) | Name v1 == Name v2 -> pure AddsSpecAny
    (SumsTo _ (Delta (Lit _ n)) cond xs@(_ : _)) -> do
      (rhsTotal, needsNeg) <- intSumWithUniqueV v1 xs
      case r of
        CoinR ->
          if needsNeg
            then pure (vRightNeg (toI n) cond rhsTotal nam)
            else pure (vRight (toI n) cond rhsTotal nam)
        DeltaCoinR ->
          if needsNeg
            then pure (vRightNeg (toI n) cond rhsTotal nam)
            else pure (vRight (toI n) cond rhsTotal nam)
        other -> failT [show predx, show other ++ " should be either Coin or DeltaCoin"]
    (SumsTo _ (Lit r2 n) cond xs@(_ : _)) -> do
      (rhsTotal, needsNeg) <- intSumWithUniqueV v1 xs
      Refl <- sameRep r r2
      if needsNeg
        then pure (vRightNeg (toI n) cond rhsTotal nam)
        else pure (vRight (toI n) cond rhsTotal nam)
    (SumsTo _ (Var v2@(V _ r2 _)) cond xs@(_ : _)) | Name v1 == Name v2 -> do
      rhsTotal <- summandsAsInt xs
      Refl <- sameRep r r2
      pure $ vLeft nam cond rhsTotal
    (SumsTo _ (Delta (Var v2)) cond xs@(_ : _)) | Name v1 == Name v2 -> do
      rhsTotal <- summandsAsInt xs
      case r of
        CoinR -> pure $ vLeft nam cond rhsTotal
        DeltaCoinR -> pure $ vLeft nam cond rhsTotal
        other -> failT [show predx, show other ++ " should be either Coin or DeltaCoin"]
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
summandAsInt (SumMap (Lit _ m)) = pure (toI (Map.foldl' add zero m))
summandAsInt (SumList (Lit _ m)) = pure (toI (List.foldl' add zero m))
summandAsInt (Project _ (Lit _ m)) = pure (toI (List.foldl' (\ans x -> add ans (getSum x)) zero m))
summandAsInt x = failT ["Can't compute summandAsInt: " ++ show x ++ ", to an Int."]

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
  (List (Var v2@(V _ (ListR r2) _)) xs) | Name v1 == Name v2 -> do
    Refl <- sameRep r r2
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
  Update :: Eq s => s -> Lens' t s -> Update t

update :: t -> [Update t] -> t
update t [] = t
update t (Update s l : more) = update (Lens.set l s t) more

anyToUpdate :: Rep era t1 -> (AnyF era t2) -> Typed (Update t1)
anyToUpdate rep1 (AnyF (FConst _ s (Yes rep2 l))) = do
  Refl <- sameRep rep1 rep2
  pure (Update s l)
anyToUpdate _ x = failT ["component is not WConst: " ++ show x]

intToNatural :: Int -> Natural
intToNatural n = fromIntegral n

-- ==================================================================================
-- Given a variable ('v1' :: 't') and [Pred] that constrain it. Produce a (Gen t)

-- | Dispatch on the type of the variable 'v1' being solved.
dispatch :: forall t era. Era era => V era t -> [Pred era] -> Typed (Gen t)
dispatch v1@(V nam r1 _) preds = explain ("Solving for variable " ++ nam ++ show preds) $ case preds of
  [Var v2 :=: Lit r2 t] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    pure (pure t)
  [Lit r2 t :=: Var v2] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    pure (pure t)
  [Sized (Var v2@(V _ r2 _)) term] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    -- Refl <- sameRep r2 SizeR
    x <- simplify term
    pure $ pure (SzExact (getSize x))
  [cc@(Component (Var v2) cs)] | Name v1 == Name v2 -> explain ("Solving " ++ show cc) $ do
    pairs <- mapM (anyToUpdate r1) cs
    pure $ do t <- genRep r1; pure (update t pairs)
  [Component (Var v2) cs, Random (Var v3)] | Name v1 == Name v2 && Name v1 == Name v3 -> do
    pairs <- mapM (anyToUpdate r1) cs
    pure $ do
      t <- genRep r1
      pure (update t pairs)
  [Random (Var v3), Component (Var v2) cs] | Name v1 == Name v2 && Name v1 == Name v3 -> do
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
  [Var v2@(V _ r2 _) :<-: target] | Name v1 == Name v2 ->
    do
      Refl <- sameRep r1 r2
      x <- simplifyTarget @era @t target
      pure (pure x)
  [Member (Var v2@(V _ r2 _)) expr] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    set <- simplify expr
    let msgs = ("Solving for variable " ++ nam) : map show preds
    pure (fst <$> (itemFromSet msgs set))
  [NotMember (Var v2@(V _ r2 _)) expr] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    set <- simplify expr
    let msgs = ("Solving for variable " ++ nam) : map show preds
    pure $ suchThatErr msgs (genRep r2) (`Set.notMember` set)
  [List (Var v2@(V _ (MaybeR r2) _)) expr] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 (MaybeR r2)
    xs <- mapM simplify expr
    pure $ (pure (makeFromList xs))
  [Random (Var v2)] | Name v1 == Name v2 -> pure $ genRep r1
  cs -> case r1 of
    MapR dom rng -> do
      spec <- solveMaps v1 cs
      pure $ genFromMapSpec v1 (map show cs) (genRep dom) (genRep rng) spec
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
            ]

genOrFail ::
  Era era =>
  Bool ->
  Either [String] (Subst era) ->
  (Name era, [Pred era]) ->
  Gen (Either [String] (Subst era))
genOrFail loud (Right subst) (Name v@(V _ rep _), conds) =
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
        (pure (Right (SubItem v (Lit rep t) : subst)))
    Left msgs -> pure (Left msgs)
genOrFail _ (Left msgs) _ = pure (Left msgs)

genOrFailList ::
  Era era =>
  Bool ->
  Either [String] (Subst era) ->
  [(Name era, [Pred era])] ->
  Gen (Either [String] (Subst era))
genOrFailList loud = foldlM' (genOrFail loud)

genDependGraph :: Bool -> Proof era -> DependGraph era -> Gen (Either [String] (Subst era))
genDependGraph loud (Shelley _) (DependGraph pairs) = genOrFailList loud (Right []) pairs
genDependGraph loud (Allegra _) (DependGraph pairs) = genOrFailList loud (Right []) pairs
genDependGraph loud (Mary _) (DependGraph pairs) = genOrFailList loud (Right []) pairs
genDependGraph loud (Alonzo _) (DependGraph pairs) = genOrFailList loud (Right []) pairs
genDependGraph loud (Babbage _) (DependGraph pairs) = genOrFailList loud (Right []) pairs
genDependGraph loud (Conway _) (DependGraph pairs) = genOrFailList loud (Right []) pairs

-- | Solve for one variable, and add its solution to the substitution
solveOneVar :: Era era => Subst era -> (Name era, [Pred era]) -> Gen (Subst era)
solveOneVar subst (Name (v@(V _ r _)), ps) = do
  genOneT <- monadTyped (dispatch v (map (substPred subst) ps)) -- Sub solution for previously solved variables
  t <- genOneT
  pure (SubItem v (Lit r t) : subst)

toolChain :: Era era => Proof era -> OrderInfo -> [Pred era] -> Gen (Env era)
toolChain _proof order cs = do
  (_count, DependGraph pairs) <- compileGen order cs
  subst <- foldlM' solveOneVar [] pairs
  monadTyped $ substToEnv subst emptyEnv

-- =======================================================================
