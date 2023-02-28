{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Solver where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (foldlM')
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Lens.Micro (Lens', (&), (.~))
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (
  Adds (..),
  Count (..),
  FromInt (fromInt),
  Sizeable (getsize),
  Sums (..),
 )
import Test.Cardano.Ledger.Constrained.Combinators
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Rewrite (DependGraph (..), OrderInfo, compile)
import Test.Cardano.Ledger.Constrained.Size (
  OrdCond (..),
  Size (..),
  negOrdCond,
  runOrdCond,
  sepsP,
 )
import Test.Cardano.Ledger.Constrained.Spec (
  MapSpec (..),
  RelSpec (..),
  RngSpec (..),
  SetSpec (..),
  SumSpec (..),
  Unique (..),
  genFromMapSpec,
  genFromRelSpec,
  genFromSetSpec,
  genFromSize,
  genFromSumSpec,
  mapSpec,
  relDisjoint,
  relSubset,
  relSuperset,
  setSpec,
  showMapSpec,
  showSetSpec,
  showSumSpec,
 )
import Test.Cardano.Ledger.Constrained.TypeRep (
  Rep (..),
  genRep,
  genSizedRep,
  synopsis,
  testEql,
  (:~:) (Refl),
 )
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (
  Mock,
  Proof (..),
  ShelleyEra,
 )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck hiding (Fixed, total)
import Prelude hiding (subtract)

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

-- | Is there an Adds instance for 't'
hasAdds :: Rep era t -> (s t) -> Typed (HasCond Adds (s t))
hasAdds IntR x = pure $ With x
hasAdds Word64R x = pure $ With x
hasAdds CoinR x = pure $ With x
hasAdds DeltaCoinR x = pure $ With x
hasAdds RationalR x = pure $ With x
hasAdds NaturalR x = pure $ With x
hasAdds r _ = failT [show r ++ " does not have Adds instance."]

isAddsType :: forall era t. Rep era t -> Bool
isAddsType rep = case hasAdds rep (Id (undefined :: t)) of
  (Typed (Right (With _))) -> True
  (Typed (Left _)) -> False

-- | Is there an Count instance for 't'
hasCount :: Rep era t -> (s t) -> Typed (HasCond Count (s t))
hasCount IntR x = pure $ With x
hasCount (ProtVerR _) x = pure $ With x
hasCount EpochR x = pure $ With x
hasCount r _ = failT [show r ++ " does not have Count instance."]

isCountType :: forall era t. Rep era t -> Bool
isCountType rep = case hasCount rep (Id (undefined :: t)) of
  (Typed (Right (With _))) -> True
  (Typed (Left _)) -> False

-- | Is there a FromInt instance for 't'
hasFromInt :: Rep era t -> (s t) -> Typed (HasCond FromInt (s t))
hasFromInt IntR x = pure $ With x
hasFromInt Word64R x = pure $ With x
hasFromInt CoinR x = pure $ With x
hasFromInt NaturalR x = pure $ With x
hasFromInt r _ = failT [show r ++ " does not have FromInt instance."]

isFromIntType :: forall era t. Rep era t -> Bool
isFromIntType rep = case hasFromInt rep (Id (undefined :: t)) of
  (Typed (Right (With _))) -> True
  (Typed (Left _)) -> False

-- ==================================================
-- Extras, simple helper functions

known :: Rep era s -> Literal era t -> Maybe s
known s (Lit r x) = case testEql s r of Nothing -> Nothing; Just Refl -> Just x

-- | Simplify and return with evidence that 'expr' has type 's'
simplifyAtType :: Rep era s -> Term era t -> Typed s
simplifyAtType r1 term = do
  t <- simplify term
  Refl <- sameRep r1 (termRep term)
  pure t

simplifySet :: Ord rng => Rep era rng -> Term era y -> Typed (HasCond Ord (Set rng))
simplifySet r1 term = do
  x <- simplify term
  Refl <- sameRep (SetR r1) (termRep term)
  pure (With x)

-- | Is the Sum a variable (of a map). Only SumMap and Project store maps.
isMapVar :: Name era -> Sum era c -> Bool
isMapVar n1 (SumMap (Var v2)) = n1 == Name v2
isMapVar n1 (Project _ (Var v2)) = n1 == Name v2
isMapVar _ _ = False

exactlyOne :: (a -> Bool) -> [a] -> Bool
exactlyOne _ [] = False
exactlyOne pp (x : xs) = pp x && all (not . pp) xs || exactlyOne pp xs

sumFromDyn :: Rep era t -> Dyn era -> Typed (HasCond Adds (Id t))
sumFromDyn rep (Dyn rep2 m) = case (testEql rep rep2) of
  Just Refl -> hasAdds rep2 (Id m)
  Nothing -> failT ["(Dyn " ++ show rep2 ++ " _) does not store expected type: " ++ show rep]

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
    genThenOverwriteA a = do b <- genRep repDom; pure (b & lensDomA .~ a)

atLeast :: (Era era, Adds c) => Rep era c -> c -> Gen c
atLeast rep c = add c <$> genRep rep

-- ================================================================
-- Solver for variables of type (Map dom rng)

solveMap :: forall dom rng era. Era era => V era (Map dom rng) -> Pred era -> Typed (MapSpec era dom rng)
solveMap v1@(V _ r@(MapR dom rng) _) predicate = explain msg $ case predicate of
  (Sized (Fixed (Lit SizeR sz)) (Var v2))
    | Name v1 == Name v2 -> mapSpec sz RelAny RngAny
  (Sized (Fixed (Lit SizeR sz)) (Dom (Var v2)))
    | Name v1 == Name v2 -> mapSpec sz RelAny RngAny
  (Var v2 :=: expr) | Name v1 == Name v2 -> do
    m1 <- simplifyAtType r expr
    With m <- hasOrd rng m1
    mapSpec SzAny (RelEqual dom (Map.keysSet m)) (RngElem rng (Map.elems m))
  (expr :=: v2@(Var _)) -> solveMap v1 (v2 :=: expr)
  -- TODO recast these in terms of Fields
  (ProjS lensbt trep (Dom (Var v2@(V _ (MapR brep _) _))) :=: Fixed (Lit (SetR srep) x)) | Name v1 == Name v2 -> do
    Refl <- sameRep dom brep
    Refl <- sameRep srep trep
    With _ <- hasOrd rng rng
    pure (MapUnique (MapR dom rng) (Unique (show predicate) (projOnDom x lensbt dom rng)))
  (ProjS lensbt trep (Dom (Var v2@(V _ (MapR brep _) _))) :=: Dom (Fixed (Lit (MapR drep _) x)))
    | Name v1 == Name v2 -> do
        Refl <- sameRep dom brep
        Refl <- sameRep drep trep
        With _ <- hasOrd rng rng
        pure (MapUnique (MapR dom rng) (Unique (show predicate) (projOnDom (Map.keysSet x) lensbt dom rng)))
  (Fixed (Lit (SetR srep) x) :=: ProjS lensbt trep (Dom (Var v2@(V _ (MapR brep _) _)))) | Name v1 == Name v2 -> do
    Refl <- sameRep dom brep
    Refl <- sameRep srep trep
    With _ <- hasOrd rng rng
    pure (MapUnique (MapR dom rng) (Unique (show predicate) (projOnDom x lensbt dom rng)))
  (expr :=: Rng (Var v2))
    | Name v1 == Name v2 -> do
        s <- simplify expr
        let SetR a = termRep expr
        Refl <- sameRep rng a
        mapSpec (SzLeast (Set.size s)) RelAny (RngRel (RelEqual rng s))
  (Rng (Var v2) :=: expr)
    | Name v1 == Name v2 -> do
        s <- simplify expr
        let SetR a = termRep expr
        Refl <- sameRep rng a
        mapSpec (SzLeast (Set.size s)) RelAny (RngRel (RelEqual rng s))
  (Rng (Var v2) :<=: expr)
    | Name v1 == Name v2 -> do
        With _ <- hasOrd rng (Id undefined)
        With n <- simplifySet rng expr
        mapSpec SzAny RelAny (RngRel (relSubset rng n))
  (Rng expr :<=: (Var v2))
    | Name v1 == Name v2 -> do
        With _ <- hasOrd rng (Id undefined)
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
  (Dom (Var v2) :<=: expr)
    | Name v1 == Name v2 -> do
        With _ <- hasOrd dom dom
        With n <- simplifySet dom expr
        mapSpec (SzMost (Set.size n)) (relSubset dom n) RngAny
  (expr :<=: Dom (Var v2))
    | Name v1 == Name v2 -> do
        With _ <- hasOrd dom dom
        With n <- simplifySet dom expr
        mapSpec (SzLeast (Set.size n)) (relSuperset dom n) RngAny
  (SumsTo cond expr xs) | exactlyOne (isMapVar (Name v1)) xs -> do
    let cRep = termRep expr
    t <- simplify expr
    With (Id tx) <- hasAdds cRep (Id t)
    explain ("Solving (" ++ show predicate ++ ")") (solveMapSummands [msg] (negOrdCond cond) v1 tx xs)
  (Random (Var v2)) | Name v1 == Name v2 -> mapSpec SzAny RelAny RngAny
  (Sized (Size sz) (Var v2)) | Name v1 == Name v2 -> mapSpec sz RelAny RngAny
  (HasDom (Var v2) expr) | Name v1 == Name v2 -> do
    With set <- simplifySet dom expr
    mapSpec (SzMost (Set.size set)) (relSubset dom set) RngAny
  (Disjoint expr (Dom (Var v2))) | Name v1 == Name v2 -> do
    With set <- simplifySet dom expr
    mapSpec SzAny (relDisjoint dom set) RngAny
  (Disjoint (Dom (Var v2)) expr) | Name v1 == Name v2 -> do
    With set <- simplifySet dom expr
    mapSpec SzAny (relDisjoint dom set) RngAny
  other -> failT ["Cannot solve map condition: " ++ show other]
  where
    msg = ("Solving for " ++ show v1 ++ " Predicate \n   " ++ show predicate)

-- | We are solving for a (V era (Map d r)). This must occor exactly once in the [Sum era c]
--   That can only happen in a (RngSum cond c) or a (RngProj cond rep c) constructor of 'Sum'
--   Because we don't know if 'c' can have negative values, we do the summation as an Integer
solveMapSummands ::
  Adds c =>
  [String] ->
  OrdCond ->
  V era (Map dom rng) ->
  c ->
  [Sum era c] ->
  Typed (MapSpec era dom rng)
solveMapSummands _ cond (V _ (MapR _ r) _) c [Project crep (Var (V _ (MapR _ r1) _))] = do
  Refl <- sameRep r r1
  mapSpec SzAny RelAny (RngProj cond crep c)
solveMapSummands _ cond (V _ (MapR _ r) _) c [SumMap (Var (V _ (MapR _ r1) _))] = do
  Refl <- sameRep r r1
  mapSpec SzAny RelAny (RngSum cond c)
solveMapSummands msg cond v c (s : ss) | isMapVar (Name v) s = solveMapSummands msg cond v c (ss ++ [s])
solveMapSummands msg cond v c (s : ss) = do
  d <- simplifySum s
  solveMapSummands msg cond v (subtract msg c d) ss
solveMapSummands msg _ v _ [] = failT (("Does not have exactly one summand with variable " ++ show (Name v)) : msg)

solveMaps :: (Era era, Ord dom) => V era (Map dom rng) -> [Pred era] -> Typed (MapSpec era dom rng)
solveMaps v@(V nm (MapR _ _) _) cs =
  explain ("\nSolving for " ++ nm ++ ", Map Predicates\n" ++ unlines (map (("  " ++) . show) cs)) $
    foldlM' accum (MapSpec SzAny RelAny RngAny) cs
  where
    accum spec cond = do
      condspec <- (solveMap v cond)
      (liftT (spec <> condspec))

-- ===========================================================
-- Solving for variables with type Set

-- | Given a variable: 'v1', with a Set type, compute a SetSpec
--   which describes the constraints implied by the Pred 'predicate'
solveSet :: V era (Set a) -> Pred era -> Typed (SetSpec era a)
solveSet v1@(V _ (SetR r) _) predicate = case predicate of
  (Sized (Size sz) (Var v2)) | Name v1 == Name v2 -> setSpec sz RelAny
  (Var v2 :=: expr) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec (SzExact (Set.size set)) (RelEqual r set)
  (expr :=: v2@(Var _)) -> solveSet v1 (v2 :=: expr)
  (Var v2 :<=: expr) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec (SzMost (Set.size set)) (relSubset r set)
  (expr :<=: Var v2) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec (SzLeast (Set.size set)) (relSuperset r set)
  (Disjoint (Var v2) expr) | Name v1 == Name v2 -> do
    With set <- simplifySet r expr
    setSpec SzAny (relDisjoint r set)
  (Disjoint expr (Var v2)) -> solveSet v1 (Disjoint (Var v2) expr)
  (Random (Var v2)) | Name v1 == Name v2 -> setSpec SzAny RelAny
  (Sized (Size sz) (Var v2)) | Name v1 == Name v2 -> setSpec sz RelAny
  (HasDom mterm (Var v2)) | Name v1 == Name v2 -> do
    let MapR _ rng = termRep mterm
    mval <- simplifyAtType (MapR r rng) mterm
    setSpec (SzExact (Map.size mval)) (RelEqual r (Map.keysSet mval))
  cond -> failT ["Can't solveSet " ++ show cond ++ " for variable " ++ show v1]

solveSets :: V era (Set a) -> [Pred era] -> Typed (SetSpec era a)
solveSets v@(V nm (SetR _) _) cs =
  explain ("\nSolving for " ++ nm ++ ", Set Predicates\n" ++ unlines (map (("  " ++) . show) cs)) $
    foldlM' accum mempty cs
  where
    accum spec cond = do
      condspec <- (solveSet v cond)
      (liftT (spec <> condspec))

-- ========================================================
-- Solving for variables with an Adds instance

solveSum :: Adds t => V era t -> Pred era -> Typed (SumSpec t)
solveSum v1@(V _ r _) c =
  let msg = ["Solving [" ++ show c ++ "] for (" ++ show v1 ++ ")"]
   in case c of
        (Sized expr (Var v2)) | Name v1 == Name v2 -> do
          n <- simplifyAtType r expr
          pure $ SumSpec CondAny (Just n) Nothing
        (Sized (Var v2) expr) | Name v1 == Name v2 -> do
          n <- simplifyAtType r expr
          pure $ SumSpec EQL Nothing (Just n)
        (expr :=: (Var v2)) | Name v1 == Name v2 -> do
          n <- simplifyAtType r expr
          pure $ SumSpec EQL (Just n) Nothing
        ((Var v2) :=: expr) | Name v1 == Name v2 -> do
          n <- simplifyAtType r expr
          pure $ SumSpec EQL Nothing (Just n)
        (expr :=: Negate (Var v2)) | Name v1 == Name v2 -> do
          Refl <- sameRep r DeltaCoinR
          DeltaCoin n <- simplifyAtType DeltaCoinR expr
          pure $ SumSpec EQL (Just (DeltaCoin (-n))) Nothing
        (Negate (Var v2) :=: expr) | Name v1 == Name v2 -> do
          Refl <- sameRep r DeltaCoinR
          DeltaCoin n <- simplifyAtType DeltaCoinR expr
          pure $ SumSpec EQL Nothing (Just (DeltaCoin (-n)))
        (Random (Var v2)) | Name v1 == Name v2 -> pure $ SumSpec EQL Nothing Nothing
        (SumsTo cond (Delta (Fixed (Lit CoinR (Coin n)))) xs@(_ : _)) -> do
          rhsTotal <- sumWithUniqueV v1 xs
          case (r, rhsTotal) of
            (DeltaCoinR, DeltaCoin m) -> pure (SumSpec (negOrdCond cond) Nothing (Just (DeltaCoin (n - m))))
            (CoinR, DeltaCoin m) -> do
              coin <- deltaToCoin (DeltaCoin (n - m))
              pure (SumSpec (negOrdCond cond) Nothing (Just coin))
            (rep, x) -> failT ["Impossible SumsTo: " ++ show rep ++ " " ++ show x]
        (SumsTo cond (Fixed (Lit CoinR (Coin n))) xs@(_ : _)) -> do
          rhsTotal <- sumWithUniqueV v1 xs
          case (r, rhsTotal) of
            (DeltaCoinR, Coin m) -> pure (SumSpec (negOrdCond cond) Nothing (Just (DeltaCoin (n - m))))
            (CoinR, Coin m) -> pure (SumSpec (negOrdCond cond) Nothing (Just (Coin (n - m))))
            (rep, x) -> failT ["Impossible SumsTo: " ++ show rep ++ " " ++ show x]
        (SumsTo cond (Fixed (Lit nrep n)) xs@(_ : _)) -> do
          Refl <- sameRep r nrep
          rhsTotal <- sumWithUniqueV v1 xs
          pure (SumSpec (negOrdCond cond) Nothing (Just (subtract msg n rhsTotal)))
        (SumsTo cond (Var v2@(V _ r2 _)) xs@(_ : _)) | Name v1 == Name v2 -> do
          Refl <- sameRep r r2
          With (Id n) <- simplifySums r xs
          pure $ SumSpec cond Nothing (Just n)
        (SumsTo cond (Delta (Var v2@(V _ CoinR _))) xs@(_ : _)) | Name v1 == Name v2 -> do
          Refl <- sameRep r CoinR
          With (Id (DeltaCoin n)) <- simplifySums DeltaCoinR xs
          pure $ SumSpec cond Nothing (Just (Coin n))
        other -> failT ["Can't solveSum " ++ show (Name v1) ++ " = " ++ show other]

-- | given a [Sum era t] 'xs', all known constants, 'add' them all up.
simplifySums :: Adds t => Rep era t -> [Sum era t] -> Typed (HasCond Adds (Id t))
simplifySums rep xs = foldlM' accum (With (Id zero)) xs
  where
    accum (With (Id n)) x = do
      v <- simplifySum x
      With (Id m) <- hasAdds rep (Id v)
      pure (With (Id (add n m)))

-- | Check that there is exactly 1 occurence of 'v',
--   and return the sum of the other terms in 'ss'
--   which should all be constants.
sumWithUniqueV :: Adds c => V era t -> [Sum era c] -> Typed c
sumWithUniqueV v@(V nam _ _) ss = do
  (c, ns) <- foldlM' (unique v) (zero, []) ss
  case ns of
    [_] -> pure c
    [] -> failT ["Failed to find the unique name: " ++ nam ++ " in" ++ show ss]
    (_ : _ : _) -> failT ["The expected unique name: " ++ nam ++ " occurs more than once in " ++ show ss]

unique :: Adds c => V era t -> (c, [Name era]) -> Sum era c -> Typed (c, [Name era])
unique v1 (c, ns) (One (Var v2)) =
  if Name v1 == Name v2
    then pure (c, Name v1 : ns)
    else failT ["Unexpected Name in 'unique' " ++ show v2]
unique v1 (c, ns) (One (Delta (Var v2@(V _ CoinR _)))) =
  if Name v1 == Name v2
    then pure (c, Name v1 : ns)
    else failT ["Unexpected Name in 'unique' " ++ show v2]
unique v1 (c, ns) (One (Negate (Var v2@(V _nam DeltaCoinR _)))) =
  if Name v1 == Name v2
    then pure (c, Name v1 : ns)
    else failT ["Unexpected Name in 'unique' " ++ show v2]
unique _ (c1, ns) sumexpr = do c2 <- simplifySum sumexpr; pure (add c1 c2, ns)

deltaToCoin :: DeltaCoin -> Typed Coin
deltaToCoin d@(DeltaCoin n) =
  if n < 0
    then failT ["DeltaCoin is negative: " ++ show d ++ ". Can't convert to Coin"]
    else pure (Coin n)

solveSums :: Adds t => V era t -> [Pred era] -> Typed (SumSpec t)
solveSums v@(V nm _ _) cs =
  explain ("\nGiven (Add c), Solving for " ++ nm ++ " :: c,  with Predicates \n" ++ unlines (map (("  " ++) . show) cs)) $
    foldlM' accum mempty cs
  where
    accum spec cond = do
      condspec <- solveSum v cond
      explain
        ("Solving Sum constraint (" ++ show cond ++ ") for variable " ++ show nm)
        (liftT (spec <> condspec))

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
  pure (genPredFromSucc succVal)
genCount v1@(V _ r1 _) [CanFollow (Var v2@(V _ r2 _)) predExpr] | Name v1 == Name v2 = do
  Refl <- sameRep r1 r2
  predVal <- simplify predExpr
  pure (genSuccFromPred predVal)
genCount v@(V _ r _) cs = failT zs
  where
    zs = ("Cannot solve: genCount " ++ show v ++ " at type " ++ show r ++ " on Predicates") : map show cs

-- | Used in solving Projections
data Update t where Update :: Eq s => s -> Lens' t s -> Update t

update :: t -> [Update t] -> t
update t [] = t
update t (Update s l : more) = update (t & l .~ s) more

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
  [Var v2 :=: Fixed (Lit r2 t)] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    pure (pure t)
  [Fixed (Lit r2 t) :=: Var v2] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    pure (pure t)
  [Sized (Var v2@(V _ r2 _)) term] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 r2
    -- Refl <- sameRep r2 SizeR
    x <- simplify term
    pure $ pure (SzExact (getsize x))
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
  [Sized (Var v2) (Fixed (Lit SizeR x))] | Name v1 == Name v2 -> do
    Refl <- sameRep r1 SizeR
    pure (pure (SzExact (getsize x)))
  [Sized (Fixed (Lit SizeR sz)) (Var v2)] | isFromIntType r1 && Name v1 == Name v2 -> do
    With _ <- hasFromInt r1 r1
    pure $ fromInt <$> genFromSize sz
  [Random (Var v2)] | Name v1 == Name v2 -> pure $ genRep r1
  cs -> case r1 of
    MapR dom rng -> do
      spec <- solveMaps v1 cs
      pure $ genFromMapSpec v1 (map show cs) (genRep dom) (genRep rng) spec
    SetR r -> do
      spec <- solveSets v1 cs
      pure $ genFromSetSpec [] (genRep r) spec
    _other
      | isAddsType r1 -> do
          With v2 <- hasAdds r1 v1
          xs <- solveSums v2 cs
          genFromSumSpec (map show cs ++ ["Predicates"]) r1 xs
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
        (pure (Right (SubItem v (Fixed (Lit rep t)) : subst)))
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
  pure (SubItem v (Fixed (Lit r t)) : subst)

toolChain :: Era era => Proof era -> OrderInfo -> [Pred era] -> Gen (Env era)
toolChain _proof order cs = do
  (DependGraph pairs) <- monadTyped $ compile order cs
  subst <- foldlM' solveOneVar [] pairs
  monadTyped $ substToEnv subst emptyEnv

-- =======================================================================
