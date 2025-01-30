{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

{-
-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif
-}

-- | This module contains the most basic parts the implementation. Essentially 
--   everything to define Specification, HasSpec, HasSimpleRep, Term, Pred,
--   and the FunctionSymbol class. It also has a few HasSpec, HasSimpleRep, and FunctionSymbol
--   instances for basic types needed to define the default types and methods of HasSpec.
--   It also supplies Eq, Pretty, and Show instances on the syntax (Term, Pred, Binder etc.)
--   because many functions require these instances. It exports functions that define the
--   user interface to the domain embedded language (constrained, forall, exists etc.).
--   And, by design, nothing more.
module Constrained.GenSpecExperiment where

import Constrained.GenericExperiment
import Constrained.BaseExperiment
import Constrained.SyntaxExperiment
import Constrained.NumSpecExperiment
import Constrained.ConformanceExperiment
import Constrained.GenT
import Constrained.Core
import Constrained.Env
import Constrained.Graph


import Debug.Trace
import Control.Monad.Identity
import Control.Monad.Writer (Writer, tell)
import Data.Kind(Type,Constraint)
import Data.Semigroup (Max (..), getMax)
import Data.Typeable
import GHC.Generics
import GHC.Stack
import Prettyprinter hiding (cat)
import Test.QuickCheck hiding (Args, Fun, forAll, Witness, witness)
import Constrained.Core(Var(..),eqVar,Evidence(..))

import Constrained.List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty)
-- import GHC.TypeLits(SSymbol,KnownSymbol,Symbol,symbolSing,symbolVal,pattern SSymbol,KnownNat)
import  GHC.TypeLits hiding(Text)
import Data.Orphans() -- instances on Symbol
import Data.Foldable(toList)
import Data.Type.Equality(TestEquality(..))
import Data.String(fromString)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe(fromMaybe)
import Data.Foldable

-- =======================================================

-- STUBS    
simplifySpec :: HasSpec a => Specification a -> Specification a
simplifySpec = undefined -- SimplfyExperiment

optimisePred :: Pred -> Pred
optimisePred = undefined -- SimplfyExperiment

computeSpec ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Specification a)
computeSpec x p = undefined

computeSpecBinder :: Binder a -> GE (Specification a)
computeSpecBinder (x :-> p) = undefined

computeSpecBinderSimplified :: Binder a -> GE (Specification a)
computeSpecBinderSimplified (x :-> p) = undefined
-- =======================================================
-- Data structures
-- =======================================================

type DependGraph = Graph Name

data SolverStage where
  SolverStage ::
    HasSpec a =>
    { stageVar :: Var a
    , stagePreds :: [Pred]
    , stageSpec :: Specification a
    } ->
    SolverStage 

instance Pretty SolverStage  where
  pretty SolverStage {..} =
    viaShow stageVar
      <+> "<-"
        /> vsep'
          ( [pretty stageSpec | not $ isTrueSpec stageSpec]
              ++ ["---" | not $ null stagePreds, not $ isTrueSpec stageSpec]
              ++ map pretty stagePreds
          )

data SolverPlan = SolverPlan
  { solverPlan :: [SolverStage ]
  , solverDependencies :: Graph Name
  }

instance Pretty SolverPlan where
  pretty SolverPlan {..} =
    "\nSolverPlan"
      /> vsep'
        [ -- "\nDependencies:" /> pretty solverDependencies, -- Might be usefull someday
          "\nLinearization:" /> prettyLinear solverPlan
        ]

-- ============================================================================

-- | Generate a value that satisfies the spec. This function can fail if the
-- spec is inconsistent, there is a dependency error, or if the underlying
-- generators are not flexible enough.
genFromSpecT ::
  forall a m. (HasCallStack, HasSpec a, MonadGenError m) => Specification a -> GenT m a
genFromSpecT (simplifySpec -> spec) = case spec of
  ExplainSpec [] s -> genFromSpecT s
  ExplainSpec es s -> push es (genFromSpecT s)
  MemberSpec as -> explain1 ("genFromSpecT on spec" ++ show spec) $ pureGen (elements (NE.toList as))
  TrueSpec -> genFromSpecT (typeSpec $ emptySpec @a)
  SuspendedSpec x p
    -- NOTE: If `x` isn't free in `p` we still have to try to generate things
    -- from `p` to make sure `p` is sat and then we can throw it away. A better
    -- approach would be to only do this in the case where we don't know if `p`
    -- is sat. The proper way to implement such a sat check is to remove
    -- sat-but-unnecessary variables in the optimiser.
    | not $ Name x `appearsIn` p -> do
        !_ <- genFromPreds mempty p
        genFromSpecT TrueSpec
    | otherwise -> do
        env <- genFromPreds mempty p
        findEnv env x
  TypeSpec s cant -> do
    mode <- getMode
    explain
      ( NE.fromList
          [ "genFromSpecT on (TypeSpec tspec cant) at type " ++ showType @a
          , "tspec = "
          , show s
          , "cant = " ++ show (short cant)
          , "with mode " ++ show mode
          ]
      )
      $
      -- TODO: we could consider giving `cant` as an argument to `genFromTypeSpec` if this
      -- starts giving us trouble.
      genFromTypeSpec s `suchThatT` (`notElem` cant)
  ErrorSpec e -> genError e


-- | Generate a satisfying `Env` for a `p : Pred fn`. The `Env` contains values for
-- all the free variables in `flattenPred p`.
genFromPreds :: forall m. MonadGenError m => Env -> Pred -> GenT m Env
-- TODO: remove this once optimisePred does a proper fixpoint computation
genFromPreds env0 (optimisePred . optimisePred -> preds) =
  {- explain1 (show $ "genFromPreds fails\nPreds are:" /> pretty preds) -} do
    -- NOTE: this is just lazy enough that the work of flattening,
    -- computing dependencies, and linearizing is memoized in
    -- properties that use `genFromPreds`.
    plan <- runGE $ prepareLinearization preds
    go env0 plan
  where
    go :: Env -> SolverPlan -> GenT m Env
    go env plan | isEmptyPlan plan = pure env
    go env plan = do
      (env', plan') <-
        explain1 (show $ "Stepping the plan:" /> vsep [pretty plan, pretty env]) $ stepPlan env plan
      go env' plan'


isTrueSpec :: Specification a -> Bool
isTrueSpec TrueSpec = True
isTrueSpec _ = False

prettyLinear :: [SolverStage] -> Doc ann
prettyLinear = vsep' . map pretty

-- | Linearize a predicate, turning it into a list of variables to solve and
-- their defining constraints such that each variable can be solved independently.
prepareLinearization :: Pred -> GE SolverPlan
prepareLinearization p = do
  let preds = concatMap saturatePred $ flattenPred p
      hints = computeHints preds
      graph = transitiveClosure $ hints <> respecting hints (foldMap computeDependencies preds)
  plan <-
    explain
      ( NE.fromList
          [ "Linearizing"
          , show $ "  preds: " <> pretty preds
          , show $ "  graph: " <> pretty graph
          ]
      )
      $ linearize preds graph
  pure $ backPropagation $ SolverPlan plan graph

prettyPlan :: HasSpec a => Specification a -> Doc ann
prettyPlan (simplifySpec -> spec)
  | SuspendedSpec _ p <- spec
  , Result plan <- prepareLinearization p =
      vsep'
        [ "Simplified spec:" /> pretty spec
        , pretty plan
        ]
  | otherwise = "Simplfied spec:" /> pretty spec

printPlan :: HasSpec a => Specification a -> IO ()
printPlan = print . prettyPlan

isEmptyPlan :: SolverPlan -> Bool
isEmptyPlan (SolverPlan plan _) = null plan

stepPlan :: MonadGenError m => Env -> SolverPlan -> GenT m (Env, SolverPlan)
stepPlan env plan@(SolverPlan [] _) = pure (env, plan)
stepPlan env (SolverPlan (SolverStage x ps spec : pl) gr) = do
  (spec', specs) <- runGE
    $ explain1
      ( show
          ( "Computing specs for variable "
              <> pretty x /> vsep' (map pretty ps)
          )
      )
    $ do
      ispecs <- mapM (computeSpec x) ps
      pure $ (fold ispecs, ispecs)
  val <-
    genFromSpecT
      ( addToErrorSpec
          ( NE.fromList
              ( ( "\nStepPlan for variable: "
                    ++ show x
                    ++ " fails to produce Specification, probably overconstrained."
                )
                  : ("Original spec " ++ show spec)
                  : "Predicates"
                  : zipWith
                    (\pred spec -> "  pred " ++ show pred ++ " -> " ++ show spec)
                    ps
                    specs
              )
          )
          (spec <> spec')
      )
  let env1 = extendEnv x val env
  pure (env1, backPropagation $ SolverPlan (substStage env1 <$> pl) (deleteNode (Name x) gr))


-- Without this code the example wouldn't work because `y` is completely unconstrained during
-- generation. With this code we essentially rewrite occurences of `cJust_ A == B` to
-- `[cJust A == B, case B of Nothing -> False; Just _ -> True]` to add extra information
-- about the variables in `B`. Consequently, `y` in the example above is
-- constrained to `MemberSpec [100 .. 102]` in the plan.
saturatePred :: forall fn. Pred -> [Pred ]
saturatePred p =
  p -- <--- if there is an Explain, it is still on 'p' here
    : case p of
      Explain _es x -> saturatePred x -- Note that the Explain is still on the original 'p', so it is not lost
      Assert (EqualPat (FromGenericPat (InjLeftPat _)) t) ->
        [toPreds t (SumSpec Nothing TrueSpec (ErrorSpec (pure "saturatePred")))]
      Assert (EqualPat (FromGenericPat (InjRightPat _)) t) ->
        [toPreds t (SumSpec Nothing (ErrorSpec (pure "saturatePred")) TrueSpec)]
      -- TODO: e.g. `elem (pair x y) (lit zs) -> elem x (lit $ map fst zs)` etc.
      _ -> []


-- | Flatten nested `Let`, `Exists`, and `Block` in a `Pred fn`. `Let` and
-- `Exists` bound variables become free in the result.
flattenPred :: Pred -> [Pred]
flattenPred pIn = go (freeVarNames pIn) [pIn]
  where
    go _ [] = []
    go fvs (p : ps) = 
         case p of
           And ps' -> go fvs (ps' ++ ps)
           -- NOTE: the order of the arguments to `==.` here are important.
           -- The whole point of `Let` is that it allows us to solve all of `t`
           -- before we solve the variables in `t`.
           Let t b -> goBinder fvs b ps (\x -> (assert (t ==. V x) :))
           Exists _ b -> goBinder fvs b ps (const id)
           When b p -> map (When b) (go fvs [p]) ++ go fvs ps
           Explain es p -> map (explanation es) (go fvs [p]) ++ go fvs ps
           _ -> p : go fvs ps

    goBinder ::
      Set Int ->
      Binder a ->
      [Pred] ->
      (HasSpec a => Var a -> [Pred] -> [Pred]) ->
      [Pred]
    goBinder fvs (x :-> p) ps k = k x' $ go (Set.insert (nameOf x') fvs) (p' : ps)
      where
        (x', p') = freshen x p fvs


computeDependencies :: Pred -> DependGraph
computeDependencies = \case
  Monitor {} -> mempty
  Subst x t p -> computeDependencies (substitutePred x t p)
  Assert t -> computeTermDependencies t
  Reifies t' t _ -> t' `irreflexiveDependencyOn` t
  ForAll set b ->
    let innerG = computeBinderDependencies b
     in innerG <> set `irreflexiveDependencyOn` nodes innerG
  x `DependsOn` y -> x `irreflexiveDependencyOn` y
  Case t bs ->
    let innerG = foldMapList (computeBinderDependencies . thing) bs
     in innerG <> t `irreflexiveDependencyOn` nodes innerG
  When b p ->
    let pG = computeDependencies p
        oG = nodes pG `irreflexiveDependencyOn` b
     in oG <> pG
  TruePred -> mempty
  FalsePred {} -> mempty
  And ps -> foldMap computeDependencies ps
  Exists _ b -> computeBinderDependencies b
  Let t b -> noDependencies t <> computeBinderDependencies b
  GenHint _ t -> noDependencies t
  Explain _ p -> computeDependencies p

-- Consider: A + B = C + D
-- We want to fail if A and B are independent.
-- Consider: A + B = A + C, A <- B
-- Here we want to consider this constraint defining for A
linearize ::
  (MonadGenError m) => [Pred] -> DependGraph -> m [SolverStage]
linearize preds graph = do
  sorted <- case topsort graph of
    Left cycle ->
      fatalError1
        ( show $
            "linearize: Dependency cycle in graph:"
              /> vsep'
                [ "cycle:" /> pretty cycle
                , "graph:" /> pretty graph
                ]
        )
    Right sorted -> pure sorted
  go sorted [(freeVarSet ps, ps) | ps <- filter isRelevantPred preds]
  where
    isRelevantPred TruePred = False
    isRelevantPred DependsOn {} = False
    isRelevantPred (Assert (Lit True)) = False
    isRelevantPred _ = True

    go [] [] = pure []
    go [] ps
      | null $ foldMap fst ps =
          case checkPredsE (pure "Linearizing fails") mempty (map snd ps) of
            Nothing -> pure []
            Just msgs -> genError msgs
      | otherwise =
          fatalError $
            NE.fromList
              [ "Dependency error in `linearize`: "
              , show $ indent 2 $ "graph: " /> pretty graph
              , show $
                  indent 2 $
                    "the following left-over constraints are not defining constraints for a unique variable:"
                      /> vsep' (map (pretty . snd) ps)
              ]
    go (n@(Name x) : ns) ps = do
      let (nps, ops) = partition (isLastVariable n . fst) ps
      (normalizeSolverStage (SolverStage x (map snd nps) mempty) :) <$> go ns ops

    isLastVariable n set = n `Set.member` set && solvableFrom n (Set.delete n set) graph


-- | Does nothing if the variable is not in the plan already.
mergeSolverStage :: SolverStage -> [SolverStage] -> [SolverStage]
mergeSolverStage (SolverStage x ps spec) plan =
  [ case eqVar x y of
      Just Refl ->
        SolverStage
          y
          (ps ++ ps')
          ( addToErrorSpec
              ( NE.fromList
                  ( [ "Solving var " ++ show x ++ " fails."
                    , "Merging the Specs"
                    , "   1. " ++ show spec
                    , "   2. " ++ show spec'
                    ]
                  )
              )
              (spec <> spec')
          )
      Nothing -> stage
  | stage@(SolverStage y ps' spec') <- plan
  ]

-- | Push as much information we can backwards through the plan.
backPropagation :: SolverPlan -> SolverPlan
backPropagation (SolverPlan plan graph) = SolverPlan (go [] (reverse plan)) graph
  where
    go acc [] = acc
    go acc (s@(SolverStage (x :: Var a) ps spec) : plan) = go (s : acc) plan'
      where
        newStages = concatMap (newStage spec) ps
        plan' = foldr mergeSolverStage plan newStages

        newStage spec (Assert (EqualPat (V x') t)) =
          termVarEqCases spec x' t
        newStage spec (Assert (EqualPat t (V x'))) =
          termVarEqCases spec x' t
        newStage _ _ = []

        termVarEqCases :: HasSpec b => Specification a -> Var b -> Term b -> [SolverStage]
        termVarEqCases (MemberSpec vs) x' t
          | Set.singleton (Name x) == freeVarSet t =
              [SolverStage x' [] $ MemberSpec (NE.nub (fmap (\v -> errorGE $ runTerm (singletonEnv x v) t) vs))]
        termVarEqCases spec x' t
          | Just Refl <- eqVar x x'
          , [Name y] <- Set.toList $ freeVarSet t
          , Result ctx <- toCtx y t =
              [SolverStage y [] (propagateSpec spec ctx)]
        termVarEqCases _ _ _ = []



-- TODO: here we can compute both the explicit hints (i.e. constraints that
-- define the order of two variables) and any whole-program smarts.
computeHints :: [Pred] -> Hints
computeHints ps =
  transitiveClosure $ fold [x `irreflexiveDependencyOn` y | DependsOn x y <- ps]

computeBinderDependencies :: Binder a -> DependGraph
computeBinderDependencies (x :-> p) =
  deleteNode (Name x) $ computeDependencies p

computeTermDependencies :: Term a -> DependGraph
computeTermDependencies = fst . computeTermDependencies'

computeTermDependencies' :: Term a -> (DependGraph, Set Name)
computeTermDependencies' (App _ args) = go args
  where
    go :: List (Term fn) as -> (DependGraph, Set Name)
    go Nil = (mempty, mempty)
    go (t :> ts) =
      let (gr, ngr) = go ts
          (tgr, ntgr) = computeTermDependencies' t
       in (ntgr `irreflexiveDependencyOn` ngr <> tgr <> gr, ngr <> ntgr)
computeTermDependencies' Lit {} = (mempty, mempty)
computeTermDependencies' (V x) = (noDependencies (Name x), Set.singleton (Name x))
