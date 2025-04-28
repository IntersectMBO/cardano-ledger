{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- Semigroup (Specification a), Monoid (Specification a)
{-# OPTIONS_GHC -Wno-orphans #-}

module Constrained.Conformance where

import Constrained.AbstractSyntax
import Constrained.Base (
  HasSpec,
  Pred,
  Specification,
  Term,
  addToErrorSpec,
  combineSpec,
  conformsTo,
  explainSpecOpt,
  forAllToList,
  memberSpecList,
  notMemberSpec,
  toPreds,
  pattern TypeSpec,
 )
import Constrained.Core (
  NonEmpty ((:|)),
  Rename (rename),
 )
import Constrained.Env (
  Env,
  extendEnv,
  singletonEnv,
 )
import Constrained.GenT (
  GE (..),
  MonadGenError (..),
  catMessageList,
  errorGE,
  explain,
  fromGE,
  runGE,
 )
import Constrained.List (
  mapList,
 )
import Constrained.PrettyUtils
import Constrained.Syntax (
  runCaseOn,
  substitutePred,
 )

import Data.List (intersect, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup (sconcat)
import Prettyprinter hiding (cat)
import Test.QuickCheck (Property, Testable, property)

-- =========================================================================

-- | Does the Pred evaluate to true under the given Env.
--   If it doesn't, some explanation appears in the failure of the monad 'm'
checkPred :: forall m. MonadGenError m => Env -> Pred -> m Bool
checkPred env = \case
  p@(ElemPred bool term xs) -> do
    v <- runTerm env term
    case (elem v xs, bool) of
      (True, True) -> pure True
      (True, False) -> fatalErrorNE ("notElemPred reduces to True" :| [show p])
      (False, True) -> fatalErrorNE ("elemPred reduces to False" :| [show p])
      (False, False) -> pure True
  Monitor {} -> pure True
  Subst x t p -> checkPred env $ substitutePred x t p
  Assert t -> runTerm env t
  GenHint {} -> pure True
  p@(Reifies t' t f) -> do
    val <- runTerm env t
    val' <- runTerm env t'
    explainNE (NE.fromList ["Reification:", "  " ++ show p]) $ pure (f val == val')
  ForAll t (x :-> p) -> do
    set <- runTerm env t
    and
      <$> sequence
        [ checkPred env' p
        | v <- forAllToList set
        , let env' = extendEnv x v env
        ]
  Case t bs -> do
    v <- runTerm env t
    runCaseOn v (mapList thing bs) (\x val ps -> checkPred (extendEnv x val env) ps)
  When bt p -> do
    b <- runTerm env bt
    if b then checkPred env p else pure True
  TruePred -> pure True
  FalsePred es -> explainNE es $ pure False
  DependsOn {} -> pure True
  And ps -> checkPreds env ps
  Let t (x :-> p) -> do
    val <- runTerm env t
    checkPred (extendEnv x val env) p
  Exists k (x :-> p) -> do
    a <- runGE $ k (errorGE . explain "checkPred: Exists" . runTerm env)
    checkPred (extendEnv x a env) p
  Explain es p -> explainNE es $ checkPred env p

checkPreds :: (MonadGenError m, Traversable t) => Env -> t Pred -> m Bool
checkPreds env ps = and <$> mapM (checkPred env) ps

checkPredPure :: Env -> Pred -> Bool
checkPredPure env p = fromGE (const False) $ checkPred env p

-- ==========================================================

-- | Like checkPred, But it takes [Pred] rather than a single Pred,
--   and it builds a much more involved explanation if it fails.
--   Does the Pred evaluate to True under the given Env?
--   If it doesn't, an involved explanation appears in the (Just message)
--   If it does, then it returns Nothing
checkPredsE ::
  NE.NonEmpty String ->
  Env ->
  [Pred] ->
  Maybe (NE.NonEmpty String)
checkPredsE msgs env ps =
  case catMaybes (fmap (checkPredE env msgs) ps) of
    [] -> Nothing
    (x : xs) -> Just (NE.nub (sconcat (x NE.:| xs)))

-- | An involved explanation for a single Pred
--   The most important explanations come when an assertion fails.
checkPredE :: Env -> NE.NonEmpty String -> Pred -> Maybe (NE.NonEmpty String)
checkPredE env msgs = \case
  p@(ElemPred bool t xs) ->
    case runTermE env t of
      Left message -> Just (msgs <> message)
      Right v -> case (elem v xs, bool) of
        (True, True) -> Nothing
        (True, False) -> Just ("notElemPred reduces to True" :| [show p])
        (False, True) -> Just ("elemPred reduces to False" :| [show p])
        (False, False) -> Nothing
  Monitor {} -> Nothing
  Subst x t p -> checkPredE env msgs $ substitutePred x t p
  Assert t -> case runTermE env t of
    Right True -> Nothing
    Right False ->
      Just
        (msgs <> pure ("Assert " ++ show t ++ " returns False") <> pure ("\nenv=\n" ++ show (pretty env)))
    Left es -> Just (msgs <> es)
  GenHint {} -> Nothing
  p@(Reifies t' t f) ->
    case runTermE env t of
      Left es -> Just (msgs <> NE.fromList ["checkPredE: Reification fails", "  " ++ show p] <> es)
      Right val -> case runTermE env t' of
        Left es -> Just (msgs <> NE.fromList ["checkPredE: Reification fails", "  " ++ show p] <> es)
        Right val' ->
          if f val == val'
            then Nothing
            else
              Just
                ( msgs
                    <> NE.fromList
                      [ "checkPredE: Reification doesn't match up"
                      , "  " ++ show p
                      , show (f val) ++ " /= " ++ show val'
                      ]
                )
  ForAll t (x :-> p) -> case runTermE env t of
    Left es -> Just $ (msgs <> NE.fromList ["checkPredE: ForAll fails to run."] <> es)
    Right set ->
      let answers =
            catMaybes
              [ checkPredE env' (pure "Some items in ForAll fail") p
              | v <- forAllToList set
              , let env' = extendEnv x v env
              ]
       in case answers of
            [] -> Nothing
            (y : ys) -> Just (NE.nub (sconcat (y NE.:| ys)))
  Case t bs -> case runTermE env t of
    Right v -> runCaseOn v (mapList thing bs) (\x val ps -> checkPredE (extendEnv x val env) msgs ps)
    Left es -> Just (msgs <> pure "checkPredE: Case fails" <> es)
  When bt p -> case runTermE env bt of
    Right b -> if b then checkPredE env msgs p else Nothing
    Left es -> Just (msgs <> pure "checkPredE: When fails" <> es)
  TruePred -> Nothing
  FalsePred es -> Just (msgs <> pure "checkPredE: FalsePred" <> es)
  DependsOn {} -> Nothing
  And ps ->
    case catMaybes (fmap (checkPredE env (pure "Some items in And  fail")) ps) of
      [] -> Nothing
      (x : xs) -> Just (msgs <> NE.nub (sconcat (x NE.:| xs)))
  Let t (x :-> p) -> case runTermE env t of
    Right val -> checkPredE (extendEnv x val env) msgs p
    Left es -> Just (msgs <> pure "checkPredE: Let fails" <> es)
  Exists k (x :-> p) ->
    let eval :: forall b. Term b -> b
        eval term = case runTermE env term of
          Right v -> v
          Left es -> error $ unlines $ NE.toList (msgs <> es)
     in case k eval of
          Result a -> checkPredE (extendEnv x a env) msgs p
          FatalError es -> Just (msgs <> catMessageList es)
          GenError es -> Just (msgs <> catMessageList es)
  Explain es p -> checkPredE env (msgs <> es) p

-- | conformsToSpec with explanation. Nothing if (conformsToSpec a spec),
--   but (Just explanations) if not(conformsToSpec a spec).
conformsToSpecE ::
  forall a.
  HasSpec a =>
  a ->
  Specification a ->
  NE.NonEmpty String ->
  Maybe (NE.NonEmpty String)
conformsToSpecE a (ExplainSpec [] s) msgs = conformsToSpecE a s msgs
conformsToSpecE a (ExplainSpec (x : xs) s) msgs = conformsToSpecE a s ((x :| xs) <> msgs)
conformsToSpecE _ TrueSpec _ = Nothing
conformsToSpecE a (MemberSpec as) msgs =
  if elem a as
    then Nothing
    else
      Just
        ( msgs
            <> NE.fromList
              ["conformsToSpecE MemberSpec case", "  " ++ show a, "  not an element of", "  " ++ show as, ""]
        )
conformsToSpecE a spec@(TypeSpec s cant) msgs =
  if notElem a cant && conformsTo a s
    then Nothing
    else
      Just
        ( msgs
            <> NE.fromList
              ["conformsToSpecE TypeSpec case", "  " ++ show a, "  (" ++ show spec ++ ")", "fails", ""]
        )
conformsToSpecE a (SuspendedSpec v ps) msgs =
  case checkPredE (singletonEnv v a) msgs ps of
    Nothing -> Nothing
    Just es -> Just (pure ("conformsToSpecE SuspendedSpec case on var " ++ show v ++ " fails") <> es)
conformsToSpecE _ (ErrorSpec es) msgs = Just (msgs <> pure "conformsToSpecE ErrorSpec case" <> es)

conformsToSpec :: HasSpec a => a -> Specification a -> Bool
conformsToSpec a x = case conformsToSpecE a x (pure "call to conformsToSpecE") of
  Nothing -> True
  Just _ -> False

satisfies :: forall a. HasSpec a => Term a -> Specification a -> Pred
satisfies e (ExplainSpec [] s) = satisfies e s
satisfies e (ExplainSpec (x : xs) s) = Explain (x :| xs) $ satisfies e s
satisfies _ TrueSpec = TruePred
satisfies e (MemberSpec nonempty) = ElemPred True e nonempty
satisfies t (SuspendedSpec x p) = Subst x t p
satisfies e (TypeSpec s cant) = case cant of
  [] -> toPreds e s
  (c : cs) -> ElemPred False e (c :| cs) <> toPreds e s
satisfies _ (ErrorSpec e) = FalsePred e

-- ==================================================================

instance HasSpec a => Semigroup (Specification a) where
  ExplainSpec es x <> y = explainSpecOpt es (x <> y)
  x <> ExplainSpec es y = explainSpecOpt es (x <> y)
  TrueSpec <> s = s
  s <> TrueSpec = s
  ErrorSpec e <> ErrorSpec e' =
    ErrorSpec
      ( e
          <> pure ("------ spec <> spec ------ @" ++ showType @a)
          <> e'
      )
  ErrorSpec e <> _ = ErrorSpec e
  _ <> ErrorSpec e = ErrorSpec e
  MemberSpec as <> MemberSpec as' =
    addToErrorSpec
      ( NE.fromList
          ["Intersecting: ", "  MemberSpec " ++ show (NE.toList as), "  MemberSpec " ++ show (NE.toList as')]
      )
      ( memberSpecList
          (nub $ intersect (NE.toList as) (NE.toList as'))
          (pure "Empty intersection")
      )
  ms@(MemberSpec as) <> ts@TypeSpec {} =
    memberSpecList
      (nub $ NE.filter (`conformsToSpec` ts) as)
      ( NE.fromList
          [ "The two " ++ showType @a ++ " Specifications are inconsistent."
          , "  " ++ show ms
          , "  " ++ show ts
          ]
      )
  TypeSpec s cant <> MemberSpec as = MemberSpec as <> TypeSpec s cant
  SuspendedSpec v p <> SuspendedSpec v' p' = SuspendedSpec v (p <> rename v' v p')
  SuspendedSpec v ps <> s = SuspendedSpec v (ps <> satisfies (V v) s)
  s <> SuspendedSpec v ps = SuspendedSpec v (ps <> satisfies (V v) s)
  TypeSpec s cant <> TypeSpec s' cant' = case combineSpec s s' of
    -- NOTE: This might look like an unnecessary case, but doing
    -- it like this avoids looping.
    TypeSpec s'' cant'' -> TypeSpec s'' (cant <> cant' <> cant'')
    s'' -> s'' <> notMemberSpec (cant <> cant')

instance HasSpec a => Monoid (Specification a) where
  mempty = TrueSpec

-- =========================================================================

-- | Collect the 'monitor' calls from a specification instantiated to the given value. Typically,
--
--   >>> quickCheck $ forAll (genFromSpec spec) $ \ x -> monitorSpec spec x $ ...
monitorSpec :: Testable p => Specification a -> a -> p -> Property
monitorSpec (SuspendedSpec x p) a =
  errorGE (monitorPred (singletonEnv x a) p) . property
monitorSpec _ _ = property

monitorPred ::
  forall m. MonadGenError m => Env -> Pred -> m (Property -> Property)
monitorPred env = \case
  ElemPred {} -> pure id -- Not sure about this, but ElemPred is a lot like Assert, so ...
  Monitor m -> pure (m $ errorGE . explain "monitorPred: Monitor" . runTerm env)
  Subst x t p -> monitorPred env $ substitutePred x t p
  Assert {} -> pure id
  GenHint {} -> pure id
  Reifies {} -> pure id
  ForAll t (x :-> p) -> do
    set <- runTerm env t
    foldr (.) id
      <$> sequence
        [ monitorPred env' p
        | v <- forAllToList set
        , let env' = extendEnv x v env
        ]
  Case t bs -> do
    v <- runTerm env t
    runCaseOn v (mapList thing bs) (\x val ps -> monitorPred (extendEnv x val env) ps)
  When b p -> do
    v <- runTerm env b
    if v then monitorPred env p else pure id
  TruePred -> pure id
  FalsePred {} -> pure id
  DependsOn {} -> pure id
  And ps -> foldr (.) id <$> mapM (monitorPred env) ps
  Let t (x :-> p) -> do
    val <- runTerm env t
    monitorPred (extendEnv x val env) p
  Exists k (x :-> p) -> do
    case k (errorGE . explain "monitorPred: Exists" . runTerm env) of
      Result a -> monitorPred (extendEnv x a env) p
      _ -> pure id
  Explain es p -> explainNE es $ monitorPred env p
