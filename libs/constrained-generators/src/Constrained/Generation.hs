{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | All the things that are necessary for generation and shrinking.
module Constrained.Generation (
  -- * Generation and shrinking
  genFromSpec,
  genFromSpecT,
  genFromSpecWithSeed,
  shrinkWithSpec,
  simplifySpec,

  -- ** Debuggin
  printPlan,
  debugSpec,
  prettyPlan,

  -- * Function Symbols
  or_,
  not_,
  injRight_,
  injLeft_,
  (==.),

  -- * Other syntax
  whenTrue,

  -- * Internals
  CountCases,
  SumW (..),
  BoolW (..),
  EqW (..),
  SumSpec (..),
  pattern SumSpec,
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.Env (Env)
import Constrained.Env qualified as Env
import Constrained.FunctionSymbol
import Constrained.GenT
import Constrained.Generic
import Constrained.Graph hiding (irreflexiveDependencyOn)
import Constrained.List
import Constrained.NumOrd
import Constrained.PrettyUtils
import Constrained.Syntax
import Control.Applicative
import Control.Monad
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable
import Data.Int
import Data.Kind
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Semigroup (Any (..), getSum)
import Data.Semigroup qualified as Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Typeable
import GHC.Stack
import GHC.TypeLits
import Prettyprinter hiding (cat)
import Test.QuickCheck hiding (Args, Fun, Witness, forAll, witness)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random hiding (left, right)
import Prelude hiding (cycle, pred)

------------------------------------------------------------------------
-- Generation, shrinking, and debugging
------------------------------------------------------------------------

-- | Generate a value that satisfies the spec. This function can fail if the
-- spec is inconsistent, there is a dependency error, or if the underlying
-- generators are not flexible enough.
genFromSpecT ::
  forall a m. (HasCallStack, HasSpec a, MonadGenError m) => Specification a -> GenT m a
genFromSpecT (simplifySpec -> spec) = case spec of
  ExplainSpec [] s -> genFromSpecT s
  ExplainSpec es s -> push es (genFromSpecT s)
  MemberSpec as -> explain ("genFromSpecT on spec" ++ show spec) $ pureGen (elements (NE.toList as))
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
        Env.find env x
  TypeSpec s cant -> do
    mode <- getMode
    explainNE
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
  ErrorSpec e -> genErrorNE e

-- | A version of `genFromSpecT` that simply errors if the generator fails
genFromSpec :: forall a. (HasCallStack, HasSpec a) => Specification a -> Gen a
genFromSpec spec = do
  res <- catchGen $ genFromSpecT @a @GE spec
  either (error . ('\n' :) . catMessages) pure res

-- | A version of `genFromSpecT` that takes a seed and a size and gives you a result
genFromSpecWithSeed ::
  forall a. (HasCallStack, HasSpec a) => Int -> Int -> Specification a -> a
genFromSpecWithSeed seed size spec = unGen (genFromSpec spec) (mkQCGen seed) size

-- ----------------------- Shrinking -------------------------------

-- | Shrink a value while preserving adherence to a `Specification`
shrinkWithSpec :: forall a. HasSpec a => Specification a -> a -> [a]
-- TODO: possibly allow for ignoring the `conformsToSpec` check in the `TypeSpec`
-- case when you know what you're doing
shrinkWithSpec (simplifySpec -> spec) a = filter (`conformsToSpec` spec) $ case spec of
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

-- Debugging --------------------------------------------------------------

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

-- | Pretty-print the plan for a `Specifcation` in the terminal for debugging
printPlan :: HasSpec a => Specification a -> IO ()
printPlan = print . prettyPlan

-- | Plan pretty-printer for debugging
prettyPlan :: HasSpec a => Specification a -> Doc ann
prettyPlan (simplifySpec -> spec)
  | SuspendedSpec _ p <- spec
  , Result plan <- prepareLinearization p =
      vsep'
        [ "Simplified spec:" /> pretty spec
        , pretty plan
        ]
  | otherwise = "Simplfied spec:" /> pretty spec

-- ---------------------- Building a plan -----------------------------------

substStage :: Env -> SolverStage -> SolverStage
substStage env (SolverStage y ps spec) = normalizeSolverStage $ SolverStage y (substPred env <$> ps) spec

normalizeSolverStage :: SolverStage -> SolverStage
normalizeSolverStage (SolverStage x ps spec) = SolverStage x ps'' (spec <> spec')
  where
    (ps', ps'') = partition ((1 ==) . Set.size . freeVarSet) ps
    spec' = fromGESpec $ computeSpec x (And ps')

-- TODO: here we can compute both the explicit hints (i.e. constraints that
-- define the order of two variables) and any whole-program smarts.
computeHints :: [Pred] -> Hints
computeHints ps =
  transitiveClosure $ fold [x `irreflexiveDependencyOn` y | DependsOn x y <- ps]

-- | Linearize a predicate, turning it into a list of variables to solve and
-- their defining constraints such that each variable can be solved independently.
prepareLinearization :: Pred -> GE SolverPlan
prepareLinearization p = do
  let preds = concatMap saturatePred $ flattenPred p
      hints = computeHints preds
      graph = transitiveClosure $ hints <> respecting hints (foldMap computeDependencies preds)
  plan <-
    explainNE
      ( NE.fromList
          [ "Linearizing"
          , show $ "  preds: " <> pretty preds
          , show $ "  graph: " <> pretty graph
          ]
      )
      $ linearize preds graph
  pure $ backPropagation $ SolverPlan plan graph

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
      Let t b -> goBinder fvs b ps (\x -> (assert (t ==. (V x)) :))
      Exists _ b -> goBinder fvs b ps (const id)
      When b pp -> map (When b) (go fvs [pp]) ++ go fvs ps
      Explain es pp -> map (explanation es) (go fvs [pp]) ++ go fvs ps
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

-- Consider: A + B = C + D
-- We want to fail if A and B are independent.
-- Consider: A + B = A + C, A <- B
-- Here we want to consider this constraint defining for A
linearize ::
  MonadGenError m => [Pred] -> DependGraph -> m [SolverStage]
linearize preds graph = do
  sorted <- case topsort graph of
    Left cycle ->
      fatalError
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
            Just msgs -> genErrorNE msgs
      | otherwise =
          fatalErrorNE $
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

------------------------------------------------------------------------
-- Simplification of Specifications
------------------------------------------------------------------------

-- | Spec simplification, use with care and don't modify the spec after using this!
simplifySpec :: HasSpec a => Specification a -> Specification a
simplifySpec spec = case applyNameHints spec of
  SuspendedSpec x p ->
    let optP = optimisePred p
     in fromGESpec $
          explain
            ("\nWhile calling simplifySpec on var " ++ show x ++ "\noptP=\n" ++ show optP ++ "\n")
            (computeSpecSimplified x optP)
  MemberSpec xs -> MemberSpec xs
  ErrorSpec es -> ErrorSpec es
  TypeSpec ts cant -> TypeSpec ts cant
  TrueSpec -> TrueSpec
  ExplainSpec es s -> explainSpec es (simplifySpec s)

-- ------- Stages of simplifying -------------------------------

-- TODO: it might be necessary to run aggressiveInlining again after the let floating etc.
optimisePred :: Pred -> Pred
optimisePred p =
  simplifyPred
    . letSubexpressionElimination
    . letFloating
    . aggressiveInlining
    . simplifyPred
    $ p

aggressiveInlining :: Pred -> Pred
aggressiveInlining pred
  | inlined = aggressiveInlining pInlined
  | otherwise = pred
  where
    (pInlined, Any inlined) = runWriter $ go (freeVars pred) [] pred

    underBinder fvs x p = fvs `without` [Name x] <> singleton (Name x) (countOf (Name x) p)

    underBinderSub :: HasSpec a => Subst -> Var a -> Subst
    underBinderSub sub x =
      [ x' := t
      | x' := t <- sub
      , isNothing $ eqVar x x'
      ]

    -- NOTE: this is safe because we only use the `Subst` when it results in a literal so there
    -- is no risk of variable capture.
    goBinder :: FreeVars -> Subst -> Binder a -> Writer Any (Binder a)
    goBinder fvs sub (x :-> p) = (x :->) <$> go (underBinder fvs x p) (underBinderSub sub x) p

    -- Check that the name `n` is only ever used as the only variable
    -- in the expressions where it appears. This ensures that it doesn't
    -- interact with anything.
    onlyUsedUniquely n p = case p of
      Assert t
        | n `appearsIn` t -> Set.size (freeVarSet t) == 1
        | otherwise -> True
      And ps -> all (onlyUsedUniquely n) ps
      -- TODO: we can (and should) probably add a bunch of cases to this.
      _ -> False

    go fvs sub pred2 = case pred2 of
      ElemPred bool t xs
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ ElemPred bool (Lit a) xs
        | otherwise -> pure $ ElemPred bool t xs
      Subst x t p -> go fvs sub (substitutePred x t p)
      Reifies t' t f
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ Reifies t' (Lit a) f
        | otherwise -> pure $ Reifies t' t f
      ForAll set b
        | not (isLit set)
        , Lit a <- substituteAndSimplifyTerm sub set -> do
            tell $ Any True
            pure $ foldMap (`unBind` b) (forAllToList a)
        | otherwise -> ForAll set <$> goBinder fvs sub b
      Case t bs
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ runCaseOn a (mapList thing bs) $ \x v p -> substPred (Env.singleton x v) p
        | (Weighted w (x :-> p) :> Nil) <- bs -> do
            let t' = substituteAndSimplifyTerm sub t
            p' <- go (underBinder fvs x p) (x := t' : sub) p
            pure $ Case t (Weighted w (x :-> p') :> Nil)
        | otherwise -> Case t <$> mapMList (traverseWeighted $ goBinder fvs sub) bs
      When b tp
        | not (isLit b)
        , Lit a <- substituteAndSimplifyTerm sub b -> do
            tell $ Any True
            pure $ if a then tp else TruePred
        | otherwise -> whenTrue b <$> go fvs sub tp
      Let t (x :-> p)
        | all (\n -> count n fvs <= 1) (freeVarSet t) -> do
            tell $ Any True
            pure $ substitutePred x t p
        | onlyUsedUniquely (Name x) p -> do
            tell $ Any True
            pure $ substitutePred x t p
        | not $ Name x `appearsIn` p -> do
            tell $ Any True
            pure p
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ unBind a (x :-> p)
        | otherwise -> Let t . (x :->) <$> go (underBinder fvs x p) (x := t : sub) p
      Exists k b -> Exists k <$> goBinder fvs sub b
      And ps -> fold <$> mapM (go fvs sub) ps
      Assert t
        | not (isLit t)
        , Lit b <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ toPred b
        | otherwise -> pure pred2
      -- If the term turns into a literal, there is no more generation to do here
      -- so we can ignore the `GenHint`
      GenHint _ t
        | not (isLit t)
        , Lit {} <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure TruePred
        | otherwise -> pure pred2
      DependsOn t t'
        | not (isLit t)
        , Lit {} <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ TruePred
        | not (isLit t')
        , Lit {} <- substituteAndSimplifyTerm sub t' -> do
            tell $ Any True
            pure $ TruePred
        | otherwise -> pure pred2
      TruePred -> pure pred2
      FalsePred {} -> pure pred2
      Monitor {} -> pure pred2
      Explain es p -> Explain es <$> go fvs sub p

-- | Apply a substitution and simplify the resulting term if the
-- substitution changed the term.
substituteAndSimplifyTerm :: Subst -> Term a -> Term a
substituteAndSimplifyTerm sub t =
  case runWriter $ substituteTerm' sub t of
    (t', Any b)
      | b -> simplifyTerm t'
      | otherwise -> t'

-- | Simplify a Term, if the Term is an 'App', apply the rewrite rules
--   chosen by the (Logic sym t bs a) instance attached
--   to the function witness 'f'
simplifyTerm :: forall a. Term a -> Term a
simplifyTerm = \case
  V v -> V v
  Lit l -> Lit l
  App (f :: t bs a) (mapList simplifyTerm -> ts)
    | Just vs <- fromLits ts -> Lit $ uncurryList_ unValue (semantics f) vs
    | Just t <- rewriteRules f ts (Evidence @(AppRequires t bs a)) -> simplifyTerm t
    | otherwise -> App f ts

simplifyPred :: Pred -> Pred
simplifyPred = \case
  -- If the term simplifies away to a literal, that means there is no
  -- more generation to do so we can get rid of `GenHint`
  GenHint h t -> case simplifyTerm t of
    Lit {} -> TruePred
    t' -> GenHint h t'
  p@(ElemPred bool t xs) -> case simplifyTerm t of
    Lit x -> case (elem x xs, bool) of
      (True, True) -> TruePred
      (True, False) -> FalsePred ("notElemPred reduces to True" :| [show p])
      (False, True) -> FalsePred ("elemPred reduces to False" :| [show p])
      (False, False) -> TruePred
    t' -> ElemPred bool t' xs
  Subst x t p -> simplifyPred $ substitutePred x t p
  Assert t -> Assert $ simplifyTerm t
  Reifies t' t f -> case simplifyTerm t of
    Lit a ->
      -- Assert $ simplifyTerm t' ==. Lit (f a)
      ElemPred True (simplifyTerm t') (pure (f a))
    t'' -> Reifies (simplifyTerm t') t'' f
  ForAll (ts :: Term t) (b :: Binder a) -> case simplifyTerm ts of
    Lit as -> foldMap (`unBind` b) (forAllToList as)
    -- (App (extractW (UnionW @t) -> Just Refl) xs) -> error "MADE IT"
    {- Has to wait until we have HasSpec(Set a) instance
    UnionPat (xs :: Term (Set a)) ys ->
       let b' = simplifyBinder b
       in mkForAll xs b' <> mkForAll ys b' -}
    set' -> case simplifyBinder b of
      _ :-> TruePred -> TruePred
      b' -> ForAll set' b'
  DependsOn _ Lit {} -> TruePred
  DependsOn Lit {} _ -> TruePred
  DependsOn x y -> DependsOn x y
  -- Here is where we need the SumSpec instance
  Case t bs -> mkCase (simplifyTerm t) (mapList (mapWeighted simplifyBinder) bs)
  When b p -> whenTrue (simplifyTerm b) (simplifyPred p)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  And ps -> fold (simplifyPreds ps)
  Let t b -> case simplifyTerm t of
    t'@App {} -> Let t' (simplifyBinder b)
    -- Variable or literal
    t' | x :-> p <- b -> simplifyPred $ substitutePred x t' p
  Exists k b -> case simplifyBinder b of
    _ :-> TruePred -> TruePred
    -- This is to get rid of exisentials like:
    -- `constrained $ \ x -> exists $ \ y -> [x ==. y, y + 2 <. 10]`
    x :-> p | Just t <- pinnedBy x p -> simplifyPred $ substitutePred x t p
    b' -> Exists k b'
  Monitor {} -> TruePred
  -- TODO: This is a bit questionable. On the one hand we could get rid of `Explain` here
  -- and just return `simplifyPred p` but doing so risks missing explanations when things
  -- do go wrong.
  Explain es p -> explanation es $ simplifyPred p

simplifyPreds :: [Pred] -> [Pred]
simplifyPreds = go [] . map simplifyPred
  where
    go acc [] = reverse acc
    go _ (FalsePred err : _) = [FalsePred err]
    go acc (TruePred : ps) = go acc ps
    go acc (p : ps) = go (p : acc) ps

simplifyBinder :: Binder a -> Binder a
simplifyBinder (x :-> p) = x :-> simplifyPred p

-- TODO: this can probably be cleaned up and generalized along with generalizing
-- to make sure we float lets in some missing cases.
letFloating :: Pred -> Pred
letFloating = fold . go []
  where
    goBlock ctx ps = goBlock' (freeVarNames ctx <> freeVarNames ps) ctx ps

    goBlock' :: Set Int -> [Pred] -> [Pred] -> [Pred]
    goBlock' _ ctx [] = ctx
    goBlock' fvs ctx (Let t (x :-> p) : ps) =
      -- We can do `goBlock'` here because we've already done let floating
      -- on the inner `p`
      [Let t (x' :-> fold (goBlock' (Set.insert (nameOf x') fvs) ctx (p' : ps)))]
      where
        (x', p') = freshen x p fvs
    goBlock' fvs ctx (And ps : ps') = goBlock' fvs ctx (ps ++ ps')
    goBlock' fvs ctx (p : ps) = goBlock' fvs (p : ctx) ps

    goExists ::
      HasSpec a =>
      [Pred] ->
      (Binder a -> Pred) ->
      Var a ->
      Pred ->
      [Pred]
    goExists ctx ex x (Let t (y :-> p))
      | not $ Name x `appearsIn` t =
          let (y', p') = freshen y p (Set.insert (nameOf x) $ freeVarNames p <> freeVarNames t)
           in go ctx (Let t (y' :-> ex (x :-> p')))
    goExists ctx ex x p = ex (x :-> p) : ctx

    pushExplain es (Let t (x :-> p)) = Let t (x :-> pushExplain es p)
    pushExplain es (And ps) = And (pushExplain es <$> ps)
    pushExplain es (Exists k (x :-> p)) =
      Exists (explainSemantics k) (x :-> pushExplain es p)
      where
        -- TODO: Unfortunately this is necessary on ghc 8.10.7
        explainSemantics ::
          forall a.
          ((forall b. Term b -> b) -> GE a) ->
          (forall b. Term b -> b) ->
          GE a
        explainSemantics k2 env = explainNE es $ k2 env
    -- TODO: possibly one wants to have a `Term` level explanation in case
    -- the `b` propagates to ErrorSpec for some reason?
    pushExplain es (When b p) = When b (pushExplain es p)
    pushExplain es p = explanation es p

    go ctx = \case
      ElemPred bool t xs -> ElemPred bool t xs : ctx
      And ps0 -> goBlock ctx (map letFloating ps0)
      Let t (x :-> p) -> goBlock ctx [Let t (x :-> letFloating p)]
      Exists k (x :-> p) -> goExists ctx (Exists k) x (letFloating p)
      Subst x t p -> go ctx (substitutePred x t p)
      Reifies t' t f -> Reifies t' t f : ctx
      Explain es p -> pushExplain es p : ctx
      -- TODO: float let through forall if possible
      ForAll t (x :-> p) -> ForAll t (x :-> letFloating p) : ctx
      -- TODO: float let through the cases if possible
      Case t bs -> Case t (mapList (mapWeighted (\(x :-> p) -> x :-> letFloating p)) bs) : ctx
      -- TODO: float let through if possible
      When b p -> When b (letFloating p) : ctx
      -- Boring cases
      Assert t -> Assert t : ctx
      GenHint h t -> GenHint h t : ctx
      DependsOn t t' -> DependsOn t t' : ctx
      TruePred -> TruePred : ctx
      FalsePred es -> FalsePred es : ctx
      Monitor m -> Monitor m : ctx

-- Common subexpression elimination but only on terms that are already let-bound.
letSubexpressionElimination :: Pred -> Pred
letSubexpressionElimination = go []
  where
    adjustSub :: HasSpec a => Var a -> Subst -> Subst
    adjustSub x sub =
      [ x' := t
      | x' := t <- sub
      , isNothing $ eqVar x x'
      , -- TODO: possibly freshen the binder where
      -- `x` appears instead?
      not $ Name x `appearsIn` t
      ]

    goBinder :: Subst -> Binder a -> Binder a
    goBinder sub (x :-> p) = x :-> go (adjustSub x sub) p

    go sub = \case
      ElemPred bool t xs -> ElemPred bool (backwardsSubstitution sub t) xs
      GenHint h t -> GenHint h (backwardsSubstitution sub t)
      And ps -> And (go sub <$> ps)
      Let t (x :-> p) -> Let t' (x :-> go (x := t' : sub') p)
        where
          t' = backwardsSubstitution sub t
          sub' = adjustSub x sub
      Exists k b -> Exists k (goBinder sub b)
      Subst x t p -> go sub (substitutePred x t p)
      Assert t -> Assert (backwardsSubstitution sub t)
      Reifies t' t f -> Reifies (backwardsSubstitution sub t') (backwardsSubstitution sub t) f
      -- NOTE: this is a tricky case. One possible thing to do here is to keep the old `DependsOn t t'`
      -- and have the new DependsOn if `backwardsSubstitution` changed something. With this semantics you
      -- risk running into unintuitive behaviour if you have something like:
      -- ```
      -- let x = y + z in
      --  {y + z `dependsOn` w
      --   assert $ w <. y + 2
      --   ...}
      -- ```
      -- This will be rewritten as:
      -- ```
      -- let x = y + z in
      --  {z `dependsOn` w
      --   assert $ w <. y + 2
      --   ...}
      -- ```
      -- which changes the dependency order of `w` and `y`. However, fixing
      -- this behaviour in turn makes it more difficult to detect when
      -- variables are no longer used after being substituted away - which
      -- blocks some other optimizations. As we strongly encourage users not to
      -- use `letBind` in their own code most users will never encounter this issue
      -- so the tradeoff is probably worth it.
      DependsOn t t' -> DependsOn (backwardsSubstitution sub t) (backwardsSubstitution sub t')
      ForAll t b -> ForAll (backwardsSubstitution sub t) (goBinder sub b)
      Case t bs -> Case (backwardsSubstitution sub t) (mapList (mapWeighted $ goBinder sub) bs)
      When b p -> When (backwardsSubstitution sub b) (go sub p)
      TruePred -> TruePred
      FalsePred es -> FalsePred es
      Monitor m -> Monitor m
      Explain es p -> Explain es $ go sub p

-- Turning Preds into Specifications. Here is where Propagation occurs ----

-- | Precondition: the `Pred` defines the `Var a`
-- Runs in `GE` in order for us to have detailed context on failure.
computeSpecSimplified ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Specification a)
computeSpecSimplified x pred3 = localGESpec $ case simplifyPred pred3 of
  ElemPred True t xs -> propagateSpec (MemberSpec xs) <$> toCtx x t
  ElemPred False (t :: Term b) xs -> propagateSpec (TypeSpec @b (emptySpec @b) (NE.toList xs)) <$> toCtx x t
  Monitor {} -> pure mempty
  GenHint h t -> propagateSpec (giveHint h) <$> toCtx x t
  Subst x' t p' -> computeSpec x (substitutePred x' t p') -- NOTE: this is impossible as it should have gone away already
  TruePred -> pure mempty
  FalsePred es -> genErrorNE es
  And ps -> do
    spec <- fold <$> mapM (computeSpecSimplified x) ps
    case spec of
      ExplainSpec es (SuspendedSpec y ps') -> pure $ explainSpec es (SuspendedSpec y $ simplifyPred ps')
      SuspendedSpec y ps' -> pure $ SuspendedSpec y $ simplifyPred ps'
      s -> pure s
  Let t b -> pure $ SuspendedSpec x (Let t b)
  Exists k b -> pure $ SuspendedSpec x (Exists k b)
  Assert (Lit True) -> pure mempty
  Assert (Lit False) -> genError (show pred3)
  Assert t -> propagateSpec (equalSpec True) <$> toCtx x t
  ForAll (Lit s) b -> fold <$> mapM (\val -> computeSpec x $ unBind val b) (forAllToList s)
  ForAll t b -> do
    bSpec <- computeSpecBinderSimplified b
    propagateSpec (fromForAllSpec bSpec) <$> toCtx x t
  Case (Lit val) bs -> runCaseOn val (mapList thing bs) $ \va vaVal psa -> computeSpec x (substPred (Env.singleton va vaVal) psa)
  Case t branches -> do
    branchSpecs <- mapMList (traverseWeighted computeSpecBinderSimplified) branches
    propagateSpec (caseSpec (Just (showType @a)) branchSpecs) <$> toCtx x t
  When (Lit b) tp -> if b then computeSpecSimplified x tp else pure TrueSpec
  -- This shouldn't happen a lot of the time because when the body is trivial we mostly get rid of the `When` entirely
  When {} -> pure $ SuspendedSpec x pred3
  Reifies (Lit a) (Lit val) f
    | f val == a -> pure TrueSpec
    | otherwise ->
        pure $
          ErrorSpec (NE.fromList ["Value does not reify to literal: " ++ show val ++ " -/> " ++ show a])
  Reifies t' (Lit val) f ->
    propagateSpec (equalSpec (f val)) <$> toCtx x t'
  Reifies Lit {} _ _ ->
    fatalErrorNE $ NE.fromList ["Dependency error in computeSpec: Reifies", "  " ++ show pred3]
  Explain es p -> do
    -- In case things crash in here we want the explanation
    s <- pushGE (NE.toList es) (computeSpecSimplified x p)
    -- This is because while we do want to propagate `explanation`s into `SuspendedSpec`
    -- we probably don't want to propagate the full "currently simplifying xyz" explanation.
    case s of
      SuspendedSpec x2 p2 -> pure $ SuspendedSpec x2 (explanation es p2)
      _ -> pure $ addToErrorSpec es s
  -- Impossible cases that should be ruled out by the dependency analysis and linearizer
  DependsOn {} ->
    fatalErrorNE $
      NE.fromList
        [ "The impossible happened in computeSpec: DependsOn"
        , "  " ++ show x
        , show $ indent 2 (pretty pred3)
        ]
  Reifies {} ->
    fatalErrorNE $
      NE.fromList
        ["The impossible happened in computeSpec: Reifies", "  " ++ show x, show $ indent 2 (pretty pred3)]
  where
    -- We want `genError` to turn into `ErrorSpec` and we want `FatalError` to turn into `FatalError`
    localGESpec ge = case ge of
      (GenError xs) -> Result $ ErrorSpec (catMessageList xs)
      (FatalError es) -> FatalError es
      (Result v) -> Result v

-- | Precondition: the `Pred fn` defines the `Var a`.
--   Runs in `GE` in order for us to have detailed context on failure.
computeSpec ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Specification a)
computeSpec x p = computeSpecSimplified x (simplifyPred p)

computeSpecBinderSimplified :: Binder a -> GE (Specification a)
computeSpecBinderSimplified (x :-> p) = computeSpecSimplified x p

-- | Turn a list of branches into a SumSpec. If all the branches fail return an ErrorSpec.
--   Note the requirement of HasSpec(SumOver).
caseSpec ::
  forall as.
  HasSpec (SumOver as) =>
  Maybe String ->
  List (Weighted (Specification)) as ->
  Specification (SumOver as)
caseSpec tString ss
  | allBranchesFail ss =
      ErrorSpec
        ( NE.fromList
            [ "When simplifying SumSpec, all branches in a caseOn" ++ sumType tString ++ " simplify to False."
            , show spec
            ]
        )
  | True = spec
  where
    spec = loop tString ss

    allBranchesFail :: forall as2. List (Weighted Specification) as2 -> Bool
    allBranchesFail Nil = error "The impossible happened in allBranchesFail"
    allBranchesFail (Weighted _ s :> Nil) = isErrorLike s
    allBranchesFail (Weighted _ s :> ss2@(_ :> _)) = isErrorLike s && allBranchesFail ss2

    loop ::
      forall as3.
      HasSpec (SumOver as3) =>
      Maybe String ->
      List (Weighted Specification) as3 ->
      Specification (SumOver as3)
    loop _ Nil = error "The impossible happened in caseSpec"
    loop _ (s :> Nil) = thing s
    loop mTypeString (s :> ss1@(_ :> _))
      | Evidence <- prerequisites @(SumOver as3) =
          (typeSpec $ SumSpecRaw mTypeString theWeights (thing s) (loop Nothing ss1))
      where
        theWeights =
          case (weight s, totalWeight ss1) of
            (Nothing, Nothing) -> Nothing
            (a, b) -> Just (fromMaybe 1 a, fromMaybe (lengthList ss1) b)

------------------------------------------------------------------------
-- SumSpec et al
------------------------------------------------------------------------

-- | The Specification for Sums.
data SumSpec a b
  = SumSpecRaw
      (Maybe String) -- A String which is the type of arg in (caseOn arg branch1 .. branchN)
      (Maybe (Int, Int))
      (Specification a)
      (Specification b)

-- | The "normal" view of t`SumSpec` that doesn't take weights into account
pattern SumSpec ::
  (Maybe (Int, Int)) -> (Specification a) -> (Specification b) -> SumSpec a b
pattern SumSpec a b c <- SumSpecRaw _ a b c
  where
    SumSpec a b c = SumSpecRaw Nothing a b c

{-# COMPLETE SumSpec #-}

sumType :: Maybe String -> String
sumType Nothing = ""
sumType (Just x) = " type=" ++ x

totalWeight :: List (Weighted f) as -> Maybe Int
totalWeight = fmap getSum . foldMapList (fmap Semigroup.Sum . weight)

-- =================================
-- Operations on Stages and Plans

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

isEmptyPlan :: SolverPlan -> Bool
isEmptyPlan (SolverPlan plan _) = null plan

stepPlan :: MonadGenError m => Env -> SolverPlan -> GenT m (Env, SolverPlan)
stepPlan env plan@(SolverPlan [] _) = pure (env, plan)
stepPlan env (SolverPlan (SolverStage x ps spec : pl) gr) = do
  (spec', specs) <- runGE
    $ explain
      ( show
          ( "Computing specs for variable "
              <> pretty x
                /> vsep' (map pretty ps)
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
                    ++ "PS = "
                    ++ unlines (map show ps)
                )
                  : ("Original spec " ++ show spec)
                  : "Predicates"
                  : zipWith
                    (\pred specx -> "  pred " ++ show pred ++ " -> " ++ show specx)
                    ps
                    specs
              )
          )
          (spec <> spec')
      )
  let env1 = Env.extend x val env
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
        explain (show $ "Stepping the plan:" /> vsep [pretty plan, pretty env]) $ stepPlan env plan
      go env' plan'

-- | Push as much information we can backwards through the plan.
backPropagation :: SolverPlan -> SolverPlan
-- backPropagation (SolverPlan _plan _graph) =
backPropagation (SolverPlan initplan graph) = SolverPlan (go [] (reverse initplan)) graph
  where
    go :: [SolverStage] -> [SolverStage] -> [SolverStage]
    go acc [] = acc
    go acc (s@(SolverStage (x :: Var a) ps spec) : plan) = go (s : acc) plan'
      where
        newStages = concatMap (newStage spec) ps
        plan' = foldr mergeSolverStage plan newStages
        -- Note use of the Term Pattern Equal
        newStage specl (Assert (Equal (V x') t)) =
          termVarEqCases specl x' t
        newStage specr (Assert (Equal t (V x'))) =
          termVarEqCases specr x' t
        newStage _ _ = []

        termVarEqCases :: HasSpec b => Specification a -> Var b -> Term b -> [SolverStage]
        termVarEqCases (MemberSpec vs) x' t
          | Set.singleton (Name x) == freeVarSet t =
              [SolverStage x' [] $ MemberSpec (NE.nub (fmap (\v -> errorGE $ runTerm (Env.singleton x v) t) vs))]
        termVarEqCases specx x' t
          | Just Refl <- eqVar x x'
          , [Name y] <- Set.toList $ freeVarSet t
          , Result ctx <- toCtx y t =
              [SolverStage y [] (propagateSpec specx ctx)]
        termVarEqCases _ _ _ = []

-- | Function symbols for `(==.)`
data EqW :: [Type] -> Type -> Type where
  EqualW :: (Eq a, HasSpec a) => EqW '[a, a] Bool

deriving instance Eq (EqW dom rng)

instance Show (EqW d r) where
  show EqualW = "==."

instance Syntax EqW where
  isInfix EqualW = True

instance Semantics EqW where
  semantics EqualW = (==)

instance Logic EqW where
  propagate f ctxt (ExplainSpec es s) = explainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate EqualW (HOLE :? Value x :> Nil) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App EqualW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate EqualW (Value x :! Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App EqualW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate EqualW (HOLE :? Value s :> Nil) spec =
    caseBoolSpec spec $ \case
      True -> equalSpec s
      False -> notEqualSpec s
  propagate EqualW (Value s :! Unary HOLE) spec =
    caseBoolSpec spec $ \case
      True -> equalSpec s
      False -> notEqualSpec s

  rewriteRules EqualW (t :> t' :> Nil) Evidence
    | t == t' = Just $ lit True
    | otherwise = Nothing

  saturate EqualW (FromGeneric (InjLeft _) :> t :> Nil) = [toPreds t (SumSpec Nothing TrueSpec (ErrorSpec (pure "saturatePred")))]
  saturate EqualW (FromGeneric (InjRight _) :> t :> Nil) = [toPreds t (SumSpec Nothing (ErrorSpec (pure "saturatePred")) TrueSpec)]
  saturate _ _ = []

infix 4 ==.

-- | Equality on the constraint-level
(==.) :: HasSpec a => Term a -> Term a -> Term Bool
(==.) = appTerm EqualW

-- | Pattern version of `(==.)` for rewrite rules
pattern Equal ::
  forall b.
  () =>
  forall a.
  (b ~ Bool, Eq a, HasSpec a) =>
  Term a ->
  Term a ->
  Term b
pattern Equal x y <-
  ( App
      (getWitness -> Just EqualW)
      (x :> y :> Nil)
    )

-- | Like @if b then p else assert True@ in constraint-land
whenTrue :: forall p. IsPred p => Term Bool -> p -> Pred
whenTrue (Lit True) (toPred -> p) = p
whenTrue (Lit False) _ = TruePred
whenTrue b (toPred -> FalsePred {}) = assert (not_ b)
whenTrue _ (toPred -> TruePred) = TruePred
whenTrue b (toPred -> p) = When b p

-- | Is the variable x pinned to some free term in p? (free term
-- meaning that all the variables in the term are free in p).
--
-- TODO: complete this with more cases!
pinnedBy :: forall a. HasSpec a => Var a -> Pred -> Maybe (Term a)
pinnedBy x (Assert (Equal t t'))
  | V x' <- t, Just Refl <- eqVar x x' = Just t'
  | V x' <- t', Just Refl <- eqVar x x' = Just t
pinnedBy x (And ps) = listToMaybe $ catMaybes $ map (pinnedBy x) ps
pinnedBy _ _ = Nothing

-- ==================================================================================================
-- TODO: generalize this to make it more flexible and extensible
--
-- The idea here is that we turn constraints into _extra_ constraints. C.f. the
-- `mapIsJust` example in `Constrained.Examples.Map`:

--    mapIsJust :: Specification BaseFn (Int, Int)
--    mapIsJust = constrained' $ \ [var| x |] [var| y |] ->
--      assert $ just_ x ==. lookup_ y (lit $ Map.fromList [(z, z) | z <- [100 .. 102]])

-- Without this code the example wouldn't work because `y` is completely unconstrained during
-- generation. With this code we essentially rewrite occurences of `just_ A == B` to
-- `[cJust A == B, case B of Nothing -> False; Just _ -> True]` to add extra information
-- about the variables in `B`. Consequently, `y` in the example above is
-- constrained to `MemberSpec [100 .. 102]` in the plan.
saturatePred :: Pred -> [Pred]
saturatePred p =
  -- [p]
  --  + ---- if there is an Explain, it is still on 'p' here
  --  |
  --  v
  p : case p of
    Explain _es x -> saturatePred x -- Note that the Explain is still on the original 'p', so it is not lost
    -- Note how the saturation is done by the 'saturate' method of the Logic class
    Assert (App sym xs) -> saturate sym xs
    _ -> []

-- ==================================================================
-- HasSpec for Sums
-- ==================================================================

guardSumSpec ::
  forall a b.
  (HasSpec a, HasSpec b, KnownNat (CountCases b)) =>
  [String] ->
  SumSpec a b ->
  Specification (Sum a b)
guardSumSpec msgs s@(SumSpecRaw tString _ sa sb)
  | isErrorLike sa
  , isErrorLike sb =
      ErrorSpec $
        NE.fromList $
          msgs ++ ["All branches in a caseOn" ++ sumType tString ++ " simplify to False.", show s]
  | otherwise = typeSpec s

instance (KnownNat (CountCases b), HasSpec a, HasSpec b) => Show (SumSpec a b) where
  show sumspec@(SumSpecRaw tstring hint l r) = case alternateShow @(Sum a b) sumspec of
    (BinaryShow _ ps) -> show $ parens (fromString ("SumSpec" ++ sumType tstring) /> vsep ps)
    NonBinary ->
      "(SumSpec"
        ++ sumType tstring
        ++ show (sumWeightL hint)
        ++ " ("
        ++ show l
        ++ ") "
        ++ show (sumWeightR hint)
        ++ " ("
        ++ show r
        ++ "))"

combTypeName :: Maybe String -> Maybe String -> Maybe String
combTypeName (Just x) (Just y) =
  if x == y then Just x else Just ("(" ++ x ++ " | " ++ y ++ ")")
combTypeName (Just x) Nothing = Just x
combTypeName Nothing (Just x) = Just x
combTypeName Nothing Nothing = Nothing

instance (HasSpec a, HasSpec b) => Semigroup (SumSpec a b) where
  SumSpecRaw t h sa sb <> SumSpecRaw t' h' sa' sb' =
    SumSpecRaw (combTypeName t t') (unionWithMaybe mergeH h h') (sa <> sa') (sb <> sb')
    where
      -- TODO: think more carefully about this, now weights like 2 2 and 10 15 give more weight to 10 15
      -- than would be the case if you had 2 2 and 2 3. But on the other hand this approach is associative
      -- whereas actually averaging the ratios is not. One could keep a list. Future work.
      mergeH (fA, fB) (fA', fB') = (fA + fA', fB + fB')

instance forall a b. (HasSpec a, HasSpec b, KnownNat (CountCases b)) => Monoid (SumSpec a b) where
  mempty = SumSpec Nothing mempty mempty

-- | How many constructors are there in this type?
type family CountCases a where
  CountCases (Sum a b) = 1 + CountCases b
  CountCases _ = 1

countCases :: forall a. KnownNat (CountCases a) => Int
countCases = fromIntegral (natVal @(CountCases a) Proxy)

-- | The HasSpec Sum instance
instance (HasSpec a, HasSpec b, KnownNat (CountCases b)) => HasSpec (Sum a b) where
  type TypeSpec (Sum a b) = SumSpec a b

  type Prerequisites (Sum a b) = (HasSpec a, HasSpec b)

  emptySpec = mempty

  combineSpec s s' = guardSumSpec ["When combining SumSpecs", "  " ++ show s, "  " ++ show s'] (s <> s')

  conformsTo (SumLeft a) (SumSpec _ sa _) = conformsToSpec a sa
  conformsTo (SumRight b) (SumSpec _ _ sb) = conformsToSpec b sb

  genFromTypeSpec (SumSpec h sa sb)
    | emptyA, emptyB = genError "genFromTypeSpec @SumSpec: empty"
    | emptyA = SumRight <$> genFromSpecT sb
    | emptyB = SumLeft <$> genFromSpecT sa
    | fA == 0, fB == 0 = genError "All frequencies 0"
    | otherwise =
        frequencyT
          [ (fA, SumLeft <$> genFromSpecT sa)
          , (fB, SumRight <$> genFromSpecT sb)
          ]
    where
      (max 0 -> fA, max 0 -> fB) = fromMaybe (1, countCases @b) h
      emptyA = isErrorLike sa
      emptyB = isErrorLike sb

  shrinkWithTypeSpec (SumSpec _ sa _) (SumLeft a) = SumLeft <$> shrinkWithSpec sa a
  shrinkWithTypeSpec (SumSpec _ _ sb) (SumRight b) = SumRight <$> shrinkWithSpec sb b

  toPreds ct (SumSpec h sa sb) =
    Case
      ct
      ( (Weighted (fst <$> h) $ bind $ \a -> satisfies a sa)
          :> (Weighted (snd <$> h) $ bind $ \b -> satisfies b sb)
          :> Nil
      )

  cardinalTypeSpec (SumSpec _ leftspec rightspec) = addSpecInt (cardinality leftspec) (cardinality rightspec)

  typeSpecHasError (SumSpec _ x y) =
    case (isErrorLike x, isErrorLike y) of
      (True, True) -> Just $ (errorLikeMessage x <> errorLikeMessage y)
      _ -> Nothing

  alternateShow (SumSpec h left right@(TypeSpec r [])) =
    case alternateShow @b r of
      (BinaryShow "SumSpec" ps) -> BinaryShow "SumSpec" ("|" <+> sumWeightL h <+> viaShow left : ps)
      (BinaryShow "Cartesian" ps) ->
        BinaryShow "SumSpec" ("|" <+> sumWeightL h <+> viaShow left : [parens ("Cartesian" /> vsep ps)])
      _ ->
        BinaryShow "SumSpec" ["|" <+> sumWeightL h <+> viaShow left, "|" <+> sumWeightR h <+> viaShow right]
  alternateShow (SumSpec h left right) =
    BinaryShow "SumSpec" ["|" <+> sumWeightL h <+> viaShow left, "|" <+> sumWeightR h <+> viaShow right]

-- ======================================
-- Here are the Logic Instances for Sum

-- | Function symbols for `injLeft_` and `injRight_`
data SumW dom rng where
  InjLeftW :: SumW '[a] (Sum a b)
  InjRightW :: SumW '[b] (Sum a b)

instance Show (SumW dom rng) where
  show InjLeftW = "injLeft_"
  show InjRightW = "injRight_"

deriving instance (Eq (SumW dom rng))

instance Syntax SumW

instance Semantics SumW where
  semantics InjLeftW = SumLeft
  semantics InjRightW = SumRight

instance Logic SumW where
  propagateTypeSpec InjLeftW (Unary HOLE) (SumSpec _ sl _) cant = sl <> foldMap notEqualSpec [a | SumLeft a <- cant]
  propagateTypeSpec InjRightW (Unary HOLE) (SumSpec _ _ sr) cant = sr <> foldMap notEqualSpec [a | SumRight a <- cant]

  propagateMemberSpec InjLeftW (Unary HOLE) es =
    case [a | SumLeft a <- NE.toList es] of
      (x : xs) -> MemberSpec (x :| xs)
      [] ->
        ErrorSpec $
          pure $
            "propMemberSpec (sumleft_ HOLE) on (MemberSpec es) with no SumLeft in es: " ++ show (NE.toList es)
  propagateMemberSpec InjRightW (Unary HOLE) es =
    case [a | SumRight a <- NE.toList es] of
      (x : xs) -> MemberSpec (x :| xs)
      [] ->
        ErrorSpec $
          pure $
            "propagate(InjRight HOLE) on (MemberSpec es) with no SumLeft in es: " ++ show (NE.toList es)

  mapTypeSpec InjLeftW ts = typeSpec $ SumSpec Nothing (typeSpec ts) (ErrorSpec (pure "mapTypeSpec InjLeftW"))
  mapTypeSpec InjRightW ts = typeSpec $ SumSpec Nothing (ErrorSpec (pure "mapTypeSpec InjRightW")) (typeSpec ts)

-- | Constructor for `Sum`
injLeft_ :: (HasSpec a, HasSpec b, KnownNat (CountCases b)) => Term a -> Term (Sum a b)
injLeft_ = appTerm InjLeftW

-- | Constructor for `Sum`
injRight_ :: (HasSpec a, HasSpec b, KnownNat (CountCases b)) => Term b -> Term (Sum a b)
injRight_ = appTerm InjRightW

-- | Pattern for building custom rewrite rules
pattern InjRight ::
  forall c.
  () =>
  forall a b.
  ( c ~ Sum a b
  , AppRequires SumW '[b] c
  ) =>
  Term b ->
  Term c
pattern InjRight x <- (App (getWitness -> Just InjRightW) (x :> Nil))

-- | Pattern for building custom rewrite rules
pattern InjLeft ::
  forall c.
  () =>
  forall a b.
  ( c ~ Sum a b
  , AppRequires SumW '[a] c
  ) =>
  Term a ->
  Term c
pattern InjLeft x <- App (getWitness -> Just InjLeftW) (x :> Nil)

sumWeightL, sumWeightR :: Maybe (Int, Int) -> Doc a
sumWeightL Nothing = "1"
sumWeightL (Just (x, _)) = fromString (show x)
sumWeightR Nothing = "1"
sumWeightR (Just (_, x)) = fromString (show x)

-- | Operations on Bool
data BoolW (dom :: [Type]) (rng :: Type) where
  NotW :: BoolW '[Bool] Bool
  OrW :: BoolW '[Bool, Bool] Bool

deriving instance Eq (BoolW dom rng)

instance Show (BoolW dom rng) where
  show NotW = "not_"
  show OrW = "or_"

boolSem :: BoolW dom rng -> FunTy dom rng
boolSem NotW = not
boolSem OrW = (||)

instance Semantics BoolW where
  semantics = boolSem

instance Syntax BoolW

-- ======= Logic instance BoolW

instance Logic BoolW where
  propagate f ctxt (ExplainSpec [] s) = propagate f ctxt s
  propagate f ctxt (ExplainSpec es s) = ExplainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate NotW (Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App NotW (v' :> Nil)) (v :-> ps)
  propagate NotW (Unary HOLE) spec =
    caseBoolSpec spec (equalSpec . not)
  propagate OrW (HOLE :<: x) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App OrW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate OrW (x :>: HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App OrW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate OrW (HOLE :<: s) spec =
    caseBoolSpec spec (okOr s)
  propagate OrW (s :>: HOLE) spec =
    caseBoolSpec spec (okOr s)

  mapTypeSpec NotW () = typeSpec ()

-- | We have something like ('constant' ||. HOLE) must evaluate to 'need'.
--   Return a (Specification Bool) for HOLE, that makes that True.
okOr :: Bool -> Bool -> Specification Bool
okOr constant need = case (constant, need) of
  (True, True) -> TrueSpec
  (True, False) ->
    ErrorSpec
      (pure ("(" ++ show constant ++ "||. HOLE) must equal False. That cannot be the case."))
  (False, False) -> MemberSpec (pure False)
  (False, True) -> MemberSpec (pure True)

-- | Disjunction on @`Term` `Bool`@, note that this will not cause backtracking during generation
or_ :: Term Bool -> Term Bool -> Term Bool
or_ = appTerm OrW

-- | Negation of booleans
not_ :: Term Bool -> Term Bool
not_ = appTerm NotW

-- ===============================================================================
-- Syntax for Solving : stages and plans

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

fromGESpec :: HasCallStack => GE (Specification a) -> Specification a
fromGESpec ge = case ge of
  Result s -> s
  GenError xs -> ErrorSpec (catMessageList xs)
  FatalError es -> error $ catMessages es
