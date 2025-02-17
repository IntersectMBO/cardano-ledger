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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
-- Semigroup (Specification a), Monoid (Specification a)
{-# OPTIONS_GHC -Wno-orphans #-}

module Constrained.Experiment.Conformance where

import Constrained.Experiment.Base
import Constrained.Experiment.Syntax

import Constrained.Core
import Constrained.Env
import Constrained.GenT
import Constrained.List
import Data.Foldable (fold)
import Data.Kind (Constraint, Type)
import Data.List (intersect, nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup (sconcat)
import Data.Typeable (Typeable)
import GHC.TypeLits hiding (Text)
import Prettyprinter hiding (cat)
import Data.Set (Set)
import qualified Data.Set as Set

-- =========================================================================

-- | Does the Pred evaluate to true under the given Env.
--   If it doesn't, some explanation appears in the failure of the monad 'm'
checkPred :: forall m. MonadGenError m => Env -> Pred -> m Bool
checkPred env = \case
  p@(ElemPred bool term xs) -> do
    v <- runTerm env term
    case (elem v xs, bool) of
      (True, True) -> pure True
      (True, False) -> fatalError ("notElemPred reduces to True" :| [show p])
      (False, True) -> fatalError ("elemPred reduces to False" :| [show p])
      (False, False) -> pure True
  Monitor {} -> pure True
  Subst x t p -> checkPred env $ substitutePred x t p
  Assert t -> runTerm env t
  GenHint {} -> pure True
  p@(Reifies t' t f) -> do
    val <- runTerm env t
    val' <- runTerm env t'
    explain (NE.fromList ["Reification:", "  " ++ show p]) $ pure (f val == val')
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
  FalsePred es -> explain es $ pure False
  DependsOn {} -> pure True
  And ps -> checkPreds env ps
  Let t (x :-> p) -> do
    val <- runTerm env t
    checkPred (extendEnv x val env) p
  Exists k (x :-> p) -> do
    a <- runGE $ k (errorGE . explain1 "checkPred: Exists" . runTerm env)
    checkPred (extendEnv x a env) p
  Explain es p -> explain es $ checkPred env p

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

-- | Functor like property for Specification, but instead of a Haskell function (a -> b),
--   it takes a function symbol (t c s '[a] b) from a to b. We had to wait until here to
--   write this because it depends on Semigroup property of Specification.
mapSpec ::
  forall c s t a b.
  (FunSym c s t '[a] b, c, HasSpec a, HasSpec b,HasSpec Bool) => t c s '[a] b -> Specification a -> Specification b
mapSpec f (ExplainSpec es s) = explainSpecOpt es (mapSpec f s)
mapSpec f TrueSpec = mapTypeSpec @_ @_ @_ @'[a] @b f (emptySpec @a)
mapSpec _ (ErrorSpec err) = ErrorSpec err
mapSpec f (MemberSpec as) = MemberSpec $ NE.nub $ fmap (semantics f) as
mapSpec f (SuspendedSpec x p) = 
  constrained $ \x' ->
    Exists (\_ -> fatalError (pure "mapSpec")) (x :-> fold [Assert $ (x' ==. appTerm f (V x)), p])
mapSpec f (TypeSpec ts cant) = mapTypeSpec @c @s @t @'[a] @b f ts <> notMemberSpec (map (semantics f) cant)

-- ================================================================================
-- Bool and equality

data BoolW (c :: Constraint) (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  NotW :: BoolW () "not_" '[Bool] Bool
  OrW :: BoolW () "or_" '[Bool, Bool] Bool
  EqualW :: BoolW (Eq a) "==." '[a, a] Bool

deriving instance Eq (BoolW c s dom rng)

instance Show (BoolW c s dom rng) where
  show NotW = "not_"
  show OrW = "or_"
  show EqualW = "==."

boolSem :: c => BoolW c sym dom rng -> FunTy dom rng
boolSem NotW = not
boolSem OrW = (||)
boolSem EqualW = (==)

instance Witness BoolW where
  semantics = boolSem

-- ======= FunSym instance EqualW(==.)

instance (HasSpec Bool, Typeable a) => FunSym (Eq a) "==." BoolW '[a, a] Bool where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence EqualW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App EqualW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context Evidence EqualW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App EqualW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context Evidence EqualW (HOLE :<> (s :: a) :<| End)) spec =
    caseBoolSpecX spec $ \case True -> equalSpec s; False -> notEqualSpec s
  propagate (Context Evidence EqualW ((s :: a) :|> HOLE :<> End)) spec =
    caseBoolSpecX spec $ \case True -> equalSpec s; False -> notEqualSpec s
  propagate ctx@(Context Evidence _ _) _ =
    ErrorSpec $ pure ("FunSym instance for EqualW with wrong number of arguments. " ++ show ctx)

infix 4 ==.
(==.) :: (HasSpec a,HasSpec Bool) => Term a -> Term a -> Term Bool
(==.) = appTerm EqualW

-- ======= FunSym instance NotW(not_)

instance (HasSpec Bool,TypeSpec Bool ~ SumSpec () ()) => FunSym () "not_" BoolW '[Bool] Bool where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence NotW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App NotW (v' :> Nil)) (v :-> ps)
  propagate (Context Evidence NotW (HOLE :<> End)) spec =
    caseBoolSpecX spec (equalSpec . not)
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for NotW with wrong number of arguments. " ++ show ctx)

  mapTypeSpec NotW (SumSpec h a b) = typeSpec $ SumSpec h b a

eqFn :: forall a. (Typeable a, Eq a, HasSpec Bool) => Fun '[a, a] Bool
eqFn = Fun (Evidence @(Eq a)) EqualW

not_ :: (HasSpec Bool,TypeSpec Bool ~ SumSpec () ()) => Term Bool -> Term Bool
not_ = appTerm NotW

-- ======= FunSym instance OrW(or_)

instance HasSpec Bool => FunSym () "or_" BoolW '[Bool, Bool] Bool where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence OrW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App OrW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context Evidence OrW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App OrW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context Evidence OrW (HOLE :<> (s :: Bool) :<| End)) spec =
    caseBoolSpecX spec (okOr s)
  propagate (Context Evidence OrW ((s :: Bool) :|> HOLE :<> End)) spec =
    caseBoolSpecX spec (okOr s)
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for OrW with wrong number of arguments. " ++ show ctx)

or_ :: HasSpec Bool => Term Bool -> Term Bool -> Term Bool
or_ = appTerm OrW

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

-- ========================================================================

caseBoolSpecX ::
  (HasSpec Bool, HasSpec a) => Specification Bool -> (Bool -> Specification a) -> Specification a
caseBoolSpecX spec cont = case possibleValues spec of
  [] -> ErrorSpec (NE.fromList ["No possible values in caseBoolSpec"])
  [b] -> cont b
  _ -> mempty
  where
    -- where possibleValues s = filter (flip conformsToSpec (simplifySpec s)) [True, False]
    -- This will always get the same result, and probably faster since running 2
    -- conformsToSpec on True and False takes less time than simplifying the spec.
    -- Since we are in TheKnot, we could keep the simplifySpec. Is there a good reason to?
    possibleValues s = filter (flip conformsToSpec s) [True, False]



-- ==================================================================
-- SumSpec is the TypeSpec for Sums, We will use it in TheKnot.hs
-- ==================================================================

-- | The Specification for Sums.
data SumSpec a b
  = SumSpecRaw
      (Maybe String) -- A String which is the type of arg in (caseOn arg branch1 .. branchN)
      (Maybe (Int, Int))
      (Specification a)
      (Specification b)

pattern SumSpec ::
  (Maybe (Int, Int)) -> (Specification a) -> (Specification b) -> SumSpec a b
pattern SumSpec a b c <- SumSpecRaw _ a b c
  where
    SumSpec a b c = SumSpecRaw Nothing a b c

{-# COMPLETE SumSpec #-}
{-# COMPLETE SumSpecRaw #-}

-- ============================================================================

-- | Flatten nested `Let`, `Exists`, and `And` in a `Pred fn`. `Let` and
-- `Exists` bound variables become free in the result.
flattenPred :: HasSpec Bool => Pred -> [Pred]
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
