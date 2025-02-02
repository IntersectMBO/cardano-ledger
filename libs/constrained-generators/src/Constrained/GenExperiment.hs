{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Constrained.GenExperiment where

-- import Constrained.GenericExperiment
-- import Constrained.WitnessExperiment
import Constrained.BaseExperiment
import Constrained.ConformanceExperiment
import Constrained.NumSpecExperiment
import Constrained.SimplifyExperiment
import Constrained.SyntaxExperiment

import Constrained.Core
import Constrained.Env
import Constrained.GenT
import Constrained.Graph hiding (dependency, irreflexiveDependencyOn, noDependencies)
import Constrained.Graph qualified as Graph
import Constrained.List
import Control.Monad
import Data.Foldable
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Typeable
import Prettyprinter hiding (cat)

--------------------------------
-- Stub

------------------------------------------------------------------------
-- Dependency Graphs
------------------------------------------------------------------------

type DependGraph = Graph.Graph Name

dependency :: HasVariables t => Name -> t -> DependGraph
dependency x (freeVarSet -> xs) = Graph.dependency x xs

irreflexiveDependencyOn ::
  forall t t'. (HasVariables t, HasVariables t') => t -> t' -> DependGraph
irreflexiveDependencyOn (freeVarSet -> xs) (freeVarSet -> ys) = Graph.irreflexiveDependencyOn xs ys

noDependencies :: HasVariables t => t -> DependGraph
noDependencies (freeVarSet -> xs) = Graph.noDependencies xs

type Hints = DependGraph

respecting :: Hints -> DependGraph -> DependGraph
respecting hints g = g `subtractGraph` opGraph hints

solvableFrom :: Name -> Set Name -> DependGraph -> Bool
solvableFrom x s g =
  let less = dependencies x g
   in s `Set.isSubsetOf` less && not (x `Set.member` less)

-------------------------------------

shrinkWithSpecX :: forall a. HasSpec a => Specification a -> a -> [a]
-- TODO: possibly allow for ignoring the `conformsToSpec` check in the `TypeSpec`
-- case when you know what you're doing
shrinkWithSpecX (simplifySpec -> spec) a = filter (`conformsToSpec` spec) $ case spec of
  ExplainSpec _ s -> shrinkWithSpec s a
  -- TODO: filter on can't if we have a known to be sound shrinker
  TypeSpec s _ -> shrinkWithTypeSpec s a
  -- TODO: The better way of doing this is to compute the dependency graph,
  -- shrink one variable at a time, and fixup the rest of the variables
  SuspendedSpec {} -> shr a
  MemberSpec {} -> shr a
  TrueSpec -> shr a
  ErrorSpec {} -> []
  where
    shr = shrinkWithTypeSpec (emptySpec @a)

shrinkFromPreds :: HasSpec a => Pred -> Var a -> a -> [a]
shrinkFromPreds p
  | Result plan <- prepareLinearization p = \x a -> listFromGE $ do
      -- NOTE: we do this to e.g. guard against bad construction functions in Exists
      xaGood <- checkPred (singletonEnv x a) p
      unless xaGood $
        fatalError1 "Trying to shrink a bad value, don't do that!"
      -- Get an `env` for the original value
      initialEnv <- envFromPred (singletonEnv x a) p
      return
        [ a'
        | -- Shrink the initialEnv
        env' <- shrinkEnvFromPlan initialEnv plan
        , -- Get the value of the constrained variable `x` in the shrunk env
        Just a' <- [lookupEnv env' x]
        , -- NOTE: this is necessary because it's possible that changing
        -- a particular value in the env during shrinking might not result
        -- in the value of `x` changing and there is no better way to know than
        -- to do this.
        a' /= a
        ]
  | otherwise = error "Bad pred"

-- Start with a valid Env for the plan and try to shrink it
shrinkEnvFromPlan :: Env -> SolverPlan -> [Env]
shrinkEnvFromPlan initialEnv SolverPlan {..} = go mempty solverPlan
  where
    go _ [] = [] -- In this case we decided to keep every variable the same so nothing to return
    go env ((substStage env -> SolverStage {..}) : plan) = do
      Just a <- [lookupEnv initialEnv stageVar]
      -- Two cases:
      --  - either we shrink this value and try to fixup every value later on in the plan or
      [ env' <> fixedEnv
        | a' <- shrinkWithSpec stageSpec a
        , let env' = extendEnv stageVar a' env
        , Just fixedEnv <- [fixupPlan env' plan]
        ]
        --  - we keep this value the way it is and try to shrink some later value
        ++ go (extendEnv stageVar a env) plan

    -- Fix the rest of the plan given an environment `env` for the plan so far
    fixupPlan env [] = pure env
    fixupPlan env ((substStage env -> SolverStage {..}) : plan) =
      case lookupEnv initialEnv stageVar >>= fixupWithSpec stageSpec of
        Nothing -> Nothing
        Just a -> fixupPlan (extendEnv stageVar a env) plan

substStage :: Env -> SolverStage -> SolverStage
substStage env (SolverStage y ps spec) = normalizeSolverStage $ SolverStage y (substPred env <$> ps) spec

normalizeSolverStage :: SolverStage -> SolverStage
normalizeSolverStage (SolverStage x ps spec) = SolverStage x ps'' (spec <> spec')
  where
    (ps', ps'') = partition ((1 ==) . Set.size . freeVarSet) ps
    spec' = fromGESpec $ computeSpec x (And ps')

-- Try to fix a value w.r.t a specification
fixupWithSpec :: forall a. HasSpec a => Specification a -> a -> Maybe a
fixupWithSpec spec a
  | a `conformsToSpec` spec = Just a
  | otherwise = case spec of
      MemberSpec (a :| _) -> Just a
      _ -> listToMaybe $ filter (`conformsToSpec` spec) (shrinkWithSpec TrueSpec a)

-- Construct an environment for all variables that show up on the top level
-- (i.e. ones bound in `let` and `exists`) from an environment for all the free
-- variables in the pred. The environment you get out of this function is
-- _bigger_ than the environment you put in. From
-- ```
-- let y = x + 1 in let z = y + 1 in foo x y z
-- ```
-- and an environment with `{x -> 1}` you would get `{x -> 1, y -> 2, z -> 3}`
-- out.
envFromPred :: Env -> Pred -> GE Env
envFromPred env p = case p of
  ElemPred _bool _term _xs -> pure env
  -- NOTE: these don't bind anything
  Assert {} -> pure env
  DependsOn {} -> pure env
  Monitor {} -> pure env
  TruePred {} -> pure env
  FalsePred {} -> pure env
  GenHint {} -> pure env
  -- NOTE: this is ok because the variables either come from an `Exists`, a `Let`, or from
  -- the top level
  Reifies {} -> pure env
  -- NOTE: variables in here shouldn't escape to the top level
  ForAll {} -> pure env
  Case {} -> pure env
  -- These can introduce binders that show up in the plan
  When _ p -> envFromPred env p
  Subst x a p -> envFromPred env (substitutePred x a p)
  Let t (x :-> p) -> do
    v <- runTerm env t
    envFromPred (extendEnv x v env) p
  Explain _ p -> envFromPred env p
  Exists c (x :-> p) -> do
    v <- c (errorGE . explain1 "envFromPred: Exists" . runTerm env)
    envFromPred (extendEnv x v env) p
  And [] -> pure env
  And (p : ps) -> do
    env' <- envFromPred env p
    envFromPred env' (And ps)

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

-- TODO: here we can compute both the explicit hints (i.e. constraints that
-- define the order of two variables) and any whole-program smarts.
computeHints :: [Pred] -> Hints
computeHints ps =
  transitiveClosure $ fold [x `irreflexiveDependencyOn` y | DependsOn x y <- ps]

-- Consider: A + B = C + D
-- We want to fail if A and B are independent.
-- Consider: A + B = A + C, A <- B
-- Here we want to consider this constraint defining for A
linearize ::
  MonadGenError m => [Pred] -> DependGraph -> m [SolverStage]
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

-- | Flatten nested `Let`, `Exists`, and `And` in a `Pred fn`. `Let` and
-- `Exists` bound variables become free in the result.
flattenPred :: Pred -> [Pred]
flattenPred pIn = go (freeVarNames pIn) [pIn]
  where
    go _ [] = []
    go fvs (p : ps) = case p of
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
  ElemPred _bool term _xs -> computeTermDependencies term
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

computeBinderDependencies :: Binder a -> DependGraph
computeBinderDependencies (x :-> p) =
  deleteNode (Name x) $ computeDependencies p

computeTermDependencies :: Term a -> DependGraph
computeTermDependencies = fst . computeTermDependencies'

computeTermDependencies' :: Term a -> (DependGraph, Set Name)
computeTermDependencies' (App _ args) = go args
  where
    go :: List Term as -> (DependGraph, Set Name)
    go Nil = (mempty, mempty)
    go (t :> ts) =
      let (gr, ngr) = go ts
          (tgr, ntgr) = computeTermDependencies' t
       in (ntgr `irreflexiveDependencyOn` ngr <> tgr <> gr, ngr <> ntgr)
computeTermDependencies' Lit {} = (mempty, mempty)
computeTermDependencies' (V x) = (noDependencies (Name x), Set.singleton (Name x))

{-
-- | A version of `genFromSpecT` that simply errors if the generator fails
genFromSpec :: forall a. (HasCallStack, HasSpec a) => Specification a -> Gen a
genFromSpec spec = do
  res <- catchGen $ genFromSpecT @a @GE spec
  either (error . show . catMessages) pure res

-- | A version of `genFromSpecT` that takes a seed and a size and gives you a result
genFromSpecWithSeed ::
  forall a. (HasCallStack, HasSpec a) => Int -> Int -> Specification a -> a
genFromSpecWithSeed seed size spec = unGen (genFromSpec spec) (mkQCGen seed) size

-- | A version of `genFromSpecT` that runs in the IO monad. Good for debugging.
debugSpec :: forall a. HasSpec a => Specification a -> IO ()
debugSpec spec = do
  ans <- generate $ genFromGenT $ inspect (genFromSpecT spec)
  let f x = putStrLn (unlines (NE.toList x))
      ok x =
        if conformsToSpec x spec
          then putStrLn "True"
          else putStrLn "False, perhaps there is an unsafeExists in the spec?"
  case ans of
    FatalError xs -> mapM_ f xs
    GenError xs -> mapM_ f xs
    Result x -> print spec >> print x >> ok x

genInverse ::
  ( MonadGenError m
  , HasSpec a
  , HasSpec b
  ) =>
  '[a] b ->
  Specification a ->
  b ->
  GenT m a
genInverse f argS x =
  let argSpec' = argS <> propagateSpecFun f (NilCtx HOLE) (equalSpec x)
   in explain
        ( NE.fromList
            [ "genInverse"
            , "  f = " ++ show f
            , show $ "  argS =" <+> pretty argS
            , "  x = " ++ show x
            , show $ "  argSpec' =" <+> pretty argSpec'
            ]
        )
        $ genFromSpecT argSpec'

-- Generating things from predicates --------------------------------------
-}

data SolverStage where
  SolverStage ::
    HasSpec a =>
    { stageVar :: Var a
    , stagePreds :: [Pred]
    , stageSpec :: Specification a
    } ->
    SolverStage

instance Pretty SolverStage where
  pretty SolverStage {..} =
    viaShow stageVar
      <+> "<-"
      /> vsep'
        ( [pretty stageSpec | not $ isTrueSpec stageSpec]
            ++ ["---" | not $ null stagePreds, not $ isTrueSpec stageSpec]
            ++ map pretty stagePreds
        )

data SolverPlan = SolverPlan
  { solverPlan :: [SolverStage]
  , solverDependencies :: Graph Name
  }

instance Pretty SolverPlan where
  pretty SolverPlan {..} =
    "\nSolverPlan"
      /> vsep'
        [ -- "\nDependencies:" /> pretty solverDependencies, -- Might be usefull someday
          "\nLinearization:" /> prettyLinear solverPlan
        ]

isTrueSpec :: Specification a -> Bool
isTrueSpec TrueSpec = True
isTrueSpec _ = False

prettyLinear :: [SolverStage] -> Doc ann
prettyLinear = vsep' . map pretty

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

short :: forall a x. (Show a, Typeable a) => [a] -> Doc x
short [] = "[]"
short [x] =
  let raw = show x
      refined = if length raw <= 20 then raw else take 20 raw ++ " ... "
   in "[" <+> fromString refined <+> "]"
short xs = "([" <+> viaShow (length xs) <+> "elements ...] @" <> prettyType @a <> ")"

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

-- ==============================================
-- DEAL with Patterns
-- ==============================================

-- | Push as much information we can backwards through the plan.
backPropagation :: SolverPlan -> SolverPlan
backPropagation (SolverPlan _plan _graph) = undefined

{- Depends on Patterns
backPropagation (SolverPlan plan graph) = SolverPlan (go [] (reverse plan)) graph
  where
    go acc [] = acc
    go acc (s@(SolverStage (x :: Var a) ps spec) : plan) = go (s : acc) plan'
      where
        newStages = concatMap (newStage spec) ps
        plan' = foldr mergeSolverStage plan newStages

        newStage spec (Assert (Eql (V x') t)) =
          termVarEqCases spec x' t
        newStage spec (Assert (Eql t (V x'))) =
          termVarEqCases spec x' t
        newStage _ _ = []

        termVarEqCases :: HasSpec b => Specification a -> Var b -> Term b -> [SolverStage ]
        termVarEqCases (MemberSpec vs) x' t
          | Set.singleton (Name x) == freeVarSet t =
              [SolverStage x' [] $ MemberSpec (NE.nub (fmap (\v -> errorGE $ runTerm (singletonEnv x v) t) vs))]
        termVarEqCases spec x' t
          | Just Refl <- eqVar x x'
          , [Name y] <- Set.toList $ freeVarSet t
          , Result ctx <- toCtx y t =
              [SolverStage y [] (propagateSpec spec ctx)]
        termVarEqCases _ _ _ = []
-}

-- TODO: generalize this to make it more flexible and extensible
--
-- The idea here is that we turn constraints into _extra_ constraints. C.f. the
-- `mapIsJust` example in `Constrained.Examples.Map`:

--    mapIsJust :: Specification BaseFn (Int, Int)
--    mapIsJust = constrained' $ \ [var| x |] [var| y |] ->
--      assert $ cJust_ x ==. lookup_ y (lit $ Map.fromList [(z, z) | z <- [100 .. 102]])

-- Without this code the example wouldn't work because `y` is completely unconstrained during
-- generation. With this code we essentially rewrite occurences of `cJust_ A == B` to
-- `[cJust A == B, case B of Nothing -> False; Just _ -> True]` to add extra information
-- about the variables in `B`. Consequently, `y` in the example above is
-- constrained to `MemberSpec [100 .. 102]` in the plan.
saturatePred :: Pred -> [Pred]
saturatePred _p = undefined

{-- Depends on Patterns
  p -- <--- if there is an Explain, it is still on 'p' here
    : case p of
      Explain _es x -> saturatePred x -- Note that the Explain is still on the original 'p', so it is not lost
      Assert (Eql (FromG (SLeft _)) t) ->
        [toPreds t (SumSpec Nothing TrueSpec (ErrorSpec (pure "saturatePred")))]
      Assert (Eql (FromG (SRight _)) t) ->
        [toPreds t (SumSpec Nothing (ErrorSpec (pure "saturatePred")) TrueSpec)]
      -- TODO: e.g. `elem (pair x y) (lit zs) -> elem x (lit $ map fst zs)` etc.
      _ -> []
}

pattern Eql :: forall fn. () => forall a. HasSpec a => Term a -> Term a -> Term Bool
pattern Eql a b <- App (extractFn @(EqFn fn) -> Just Equal) (a :> b :> Nil)

pattern FromG ::
  forall a.
  () =>
  (HasSpec a, HasSimpleRep a, TypeSpec a ~ TypeSpec (SimpleRep a)) =>
  Term (SimpleRep a) ->
  Term a
pattern FromG a <- App (extractFn @(GenericsFn fn) -> Just FromGeneric) (a :> Nil)

pattern SLeft ::
  forall a. () => forall b c. (HasSpec b, a ~ Sum b c) => Term b -> Term a
pattern SLeft a <- App (extractFn @(SumFn fn) -> Just InjLeft) (a :> Nil)

pattern SRight ::
  forall a. () => forall b c. (HasSpec c, a ~ Sum b c) => Term c -> Term a
pattern SRight a <- App (extractFn @(SumFn fn) -> Just InjRight) (a :> Nil)

-}

{-
-- ====================================================
-- Term patterns on function symbols on Sets, Lists
-- they are used in this
-- instance HasSpec a => Pretty (WithPrec (Term a)) where ...
-- Which uses: short :: forall a x. (Show a, Typeable a) => [a] -> Doc x
-- To elide large arguments to terms such as (subset_ x (lit y))
-- If 'y' is large, it will be elided.

pattern SubsetPat ::
  forall a.
  () =>
  forall b.
  (Ord b, HasSpec (Set b), Show b, Typeable b, a ~ Bool) =>
  Term (Set b) -> Term (Set b) -> Term a
pattern SubsetPat a b <- App (extractFn @(SetFn fn) -> Just Subset) (a :> b :> Nil)

pattern DisjointPat ::
  forall a.
  () =>
  forall b.
  (Ord b, HasSpec (Set b), Show b, Typeable b, a ~ Bool) =>
  Term (Set b) -> Term (Set b) -> Term a
pattern DisjointPat a b <- App (extractFn @(SetFn fn) -> Just Disjoint) (a :> b :> Nil)

pattern UnionPat ::
  forall a.
  () =>
  forall b.
  (Ord b, HasSpec (Set b), Show b, Typeable b, a ~ Set b) =>
  Term (Set b) -> Term (Set b) -> Term a
pattern UnionPat a b <- App (extractFn @(SetFn fn) -> Just Union) (a :> b :> Nil)

pattern MemberPat ::
  forall a.
  () =>
  forall b.
  (HasSpec b, HasSpec (Set b), Show b, Typeable b, a ~ Bool) =>
  Term b -> Term (Set b) -> Term a
pattern MemberPat a b <- App (extractFn @(SetFn fn) -> Just Member) (a :> b :> Nil)

pattern ElemPat ::
  forall a.
  () =>
  forall b.
  (HasSpec b, HasSpec [b], Show b, Typeable b, a ~ Bool) =>
  Term b -> Term [b] -> Term a
pattern ElemPat a b <- App (extractFn @(SetFn fn) -> Just Elem) (a :> b :> Nil)

pattern AppendPat ::
  forall a.
  () =>
  forall b.
  (HasSpec [b], Show b, Typeable b, a ~ [b]) => Term [b] -> Term [b] -> Term a
pattern AppendPat a b <- App (extractFn @(ListFn fn) -> Just AppendFn) (a :> b :> Nil)
-}
