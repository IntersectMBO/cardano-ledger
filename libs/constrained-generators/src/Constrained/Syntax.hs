{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-- Rename instances
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains operations and tranformations on Syntax, Term, Pred, etc.
--    1) Computing Free Variables
--    2) Substitution
--    3) Renaming
--    4) internal helper functions
--    5) Syntacic only transformations
module Constrained.Syntax where

import Constrained.AbstractSyntax
import Constrained.Base (
  Binder,
  Forallable,
  HasGenHint,
  HasSpec,
  Hint,
  HintF (..),
  IsPred,
  Pred,
  Specification,
  Term,
  bind,
  explainSpec,
  forAllToList,
  name,
  toPred,
 )
import Constrained.Core (
  Rename (rename),
  Value (..),
  Var (..),
  eqVar,
  freshen,
  unValue,
 )
import Constrained.Env (
  Env,
  extendEnv,
  lookupEnv,
  removeVar,
  singletonEnv,
 )
import Constrained.FunctionSymbol
import Constrained.GenT (
  GE (..),
  MonadGenError (..),
  errorGE,
  explain,
  fatalError,
 )
import Constrained.Generic (
  Sum (..),
  SumOver,
 )
import Constrained.Graph (
  deleteNode,
  dependencies,
  nodes,
  opGraph,
  subtractGraph,
 )
import Constrained.Graph qualified as Graph
import Constrained.List (
  List (..),
  foldMapList,
  mapList,
  mapListC,
  mapMList,
  uncurryList_,
 )
import Control.Monad.Writer (Writer, tell)
import Data.Foldable (fold, toList)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid qualified as Monoid
import Data.Orphans ()
import Data.Semigroup (Any (..))
import Data.Semigroup qualified as Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Typeable
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Prettyprinter hiding (cat)
import Test.QuickCheck hiding (Args, Fun, Witness, forAll, witness)
import Prelude hiding (pred)

-- ==========================================================
-- Variables
-- ==========================================================

mkNamed :: String -> TH.Q TH.Pat
mkNamed x =
  pure $
    TH.ViewP (TH.AppE (TH.VarE $ TH.mkName "name") (TH.LitE $ TH.StringL x)) (TH.VarP $ TH.mkName x)

mkNamedExpr :: String -> TH.Q TH.Exp
mkNamedExpr x =
  pure $
    TH.AppE (TH.AppE (TH.VarE $ TH.mkName "name") (TH.LitE $ TH.StringL x)) (TH.VarE $ TH.mkName x)

var :: TH.QuasiQuoter
var =
  TH.QuasiQuoter
    { -- Parses variables e.g. `constrained $ \ [var| x |] [var| y |] -> ...` from the strings " x " and " y "
      -- and replaces them with `name "x" -> x` and `name "y" -> y`
      TH.quotePat = mkNamed . varName
    , -- Parses variables in expressions like `assert $ [var| x |] + 3 <. 10` and replaces them with `name "x" x`
      TH.quoteExp = mkNamedExpr . varName
    , TH.quoteDec = const $ fail "var should only be used at binding sites and in expressions"
    , TH.quoteType = const $ fail "var should only be used at binding sites and in expressions"
    }
  where
    varName s = case words s of
      [w] -> w
      _ -> fail "expected a single var name"

-- ============================================================
-- 1) Free variables and variable names
-- ============================================================

freeVarNames :: forall t. HasVariables t => t -> Set Int
freeVarNames = Set.mapMonotonic (\(Name v) -> nameOf v) . freeVarSet

newtype FreeVars = FreeVars {unFreeVars :: Map Name Int}
  deriving (Show)

restrictedTo :: FreeVars -> Set Name -> FreeVars
restrictedTo (FreeVars m) nms = FreeVars $ Map.restrictKeys m nms

memberOf :: Name -> FreeVars -> Bool
memberOf n (FreeVars m) = Map.member n m

count :: Name -> FreeVars -> Int
count n (FreeVars m) = fromMaybe 0 $ Map.lookup n m

instance Semigroup FreeVars where
  FreeVars fv <> FreeVars fv' = FreeVars $ Map.unionWith (+) fv fv'

instance Monoid FreeVars where
  mempty = FreeVars mempty

freeVar :: Name -> FreeVars
freeVar n = singleton n 1

singleton :: Name -> Int -> FreeVars
singleton n k = FreeVars $ Map.singleton n k

without :: Foldable t => FreeVars -> t Name -> FreeVars
without (FreeVars m) remove = FreeVars $ foldr Map.delete m (toList remove)

class HasVariables a where
  freeVars :: a -> FreeVars
  freeVarSet :: a -> Set Name
  freeVarSet = Map.keysSet . unFreeVars . freeVars
  countOf :: Name -> a -> Int
  countOf n = count n . freeVars
  appearsIn :: Name -> a -> Bool
  appearsIn n = (> 0) . count n . freeVars

instance (HasVariables a, HasVariables b) => HasVariables (a, b) where
  freeVars (a, b) = freeVars a <> freeVars b
  freeVarSet (a, b) = freeVarSet a <> freeVarSet b
  countOf n (a, b) = countOf n a + countOf n b
  appearsIn n (a, b) = appearsIn n a || appearsIn n b

instance HasVariables (List Term as) where
  freeVars Nil = mempty
  freeVars (x :> xs) = freeVars x <> freeVars xs
  freeVarSet Nil = mempty
  freeVarSet (x :> xs) = freeVarSet x <> freeVarSet xs
  countOf _ Nil = 0
  countOf n (x :> xs) = countOf n x + countOf n xs
  appearsIn _ Nil = False
  appearsIn n (x :> xs) = appearsIn n x || appearsIn n xs

instance HasVariables Name where
  freeVars = freeVar
  freeVarSet = Set.singleton
  countOf n n'
    | n == n' = 1
    | otherwise = 0
  appearsIn n n' = n == n'

instance HasVariables (Term a) where
  freeVars = \case
    Lit {} -> mempty
    V x -> freeVar (Name x)
    App _ ts -> freeVars ts
  freeVarSet = \case
    Lit {} -> mempty
    V x -> freeVarSet (Name x)
    App _ ts -> freeVarSet ts
  countOf n = \case
    Lit {} -> 0
    V x -> countOf n (Name x)
    App _ ts -> countOf n ts
  appearsIn n = \case
    Lit {} -> False
    V x -> appearsIn n (Name x)
    App _ ts -> appearsIn n ts

instance HasVariables Pred where
  freeVars = \case
    ElemPred _ t _ -> freeVars t
    GenHint _ t -> freeVars t
    Subst x t p -> freeVars t <> freeVars p `without` [Name x]
    And ps -> foldMap freeVars ps
    Let t b -> freeVars t <> freeVars b
    Exists _ b -> freeVars b
    Assert t -> freeVars t
    Reifies t' t _ -> freeVars t' <> freeVars t
    DependsOn x y -> freeVars x <> freeVars y
    ForAll set b -> freeVars set <> freeVars b
    Case t bs -> freeVars t <> freeVars bs
    When b p -> freeVars b <> freeVars p
    TruePred -> mempty
    FalsePred _ -> mempty
    Monitor {} -> mempty
    Explain _ p -> freeVars p
  freeVarSet = \case
    ElemPred _ t _ -> freeVarSet t
    GenHint _ t -> freeVarSet t
    Subst x t p -> freeVarSet t <> Set.delete (Name x) (freeVarSet p)
    And ps -> foldMap freeVarSet ps
    Let t b -> freeVarSet t <> freeVarSet b
    Exists _ b -> freeVarSet b
    Assert t -> freeVarSet t
    Reifies t' t _ -> freeVarSet t' <> freeVarSet t
    DependsOn x y -> freeVarSet x <> freeVarSet y
    ForAll set b -> freeVarSet set <> freeVarSet b
    Case t bs -> freeVarSet t <> freeVarSet bs
    When b p -> freeVarSet b <> freeVarSet p
    Explain _ p -> freeVarSet p
    TruePred -> mempty
    FalsePred _ -> mempty
    Monitor {} -> mempty
  countOf n = \case
    ElemPred _ t _ -> countOf n t
    GenHint _ t -> countOf n t
    Subst x t p
      | n == Name x -> countOf n t
      | otherwise -> countOf n t + countOf n p
    And ps -> sum $ map (countOf n) ps
    Let t b -> countOf n t + countOf n b
    Exists _ b -> countOf n b
    Assert t -> countOf n t
    Reifies t' t _ -> countOf n t' + countOf n t
    DependsOn x y -> countOf n x + countOf n y
    ForAll set b -> countOf n set + countOf n b
    Case t bs -> countOf n t + countOf n bs
    When b p -> countOf n b + countOf n p
    Explain _ p -> countOf n p
    TruePred -> 0
    FalsePred _ -> 0
    Monitor {} -> 0
  appearsIn n = \case
    ElemPred _ t _ -> appearsIn n t
    GenHint _ t -> appearsIn n t
    Subst x t p
      | n == Name x -> appearsIn n t
      | otherwise -> appearsIn n t || appearsIn n p
    And ps -> any (appearsIn n) ps
    Let t b -> appearsIn n t || appearsIn n b
    Exists _ b -> appearsIn n b
    Assert t -> appearsIn n t
    Reifies t' t _ -> appearsIn n t' || appearsIn n t
    DependsOn x y -> appearsIn n x || appearsIn n y
    ForAll set b -> appearsIn n set || appearsIn n b
    Case t bs -> appearsIn n t || appearsIn n bs
    When b p -> appearsIn n b || appearsIn n p
    Explain _ p -> appearsIn n p
    TruePred -> False
    FalsePred _ -> False
    Monitor {} -> False

instance HasVariables (Binder a) where
  freeVars (x :-> p) = freeVars p `without` [Name x]
  freeVarSet (x :-> p) = Set.delete (Name x) (freeVarSet p)
  countOf n (x :-> p)
    | Name x == n = 0
    | otherwise = countOf n p
  appearsIn n (x :-> p)
    | Name x == n = False
    | otherwise = appearsIn n p

instance HasVariables (f a) => HasVariables (Weighted f a) where
  freeVars = freeVars . thing
  freeVarSet = freeVarSet . thing
  countOf n = countOf n . thing
  appearsIn n = appearsIn n . thing

instance HasVariables (List (Weighted Binder) as) where
  freeVars Nil = mempty
  freeVars (a :> as) = freeVars a <> freeVars as
  freeVarSet Nil = mempty
  freeVarSet (a :> as) = freeVarSet a <> freeVarSet as
  countOf _ Nil = 0
  countOf n (x :> xs) = countOf n x + countOf n xs
  appearsIn _ Nil = False
  appearsIn n (x :> xs) = appearsIn n x || appearsIn n xs

instance {-# OVERLAPPABLE #-} (Foldable t, HasVariables a) => HasVariables (t a) where
  freeVars = foldMap freeVars
  freeVarSet = foldMap freeVarSet
  countOf n = Monoid.getSum . foldMap (Monoid.Sum . countOf n)
  appearsIn n = any (appearsIn n)

instance HasVariables a => HasVariables (Set a) where
  freeVars = foldMap freeVars
  freeVarSet = foldMap freeVarSet
  countOf n = sum . Set.map (countOf n)
  appearsIn n = any (appearsIn n)

-- =================================================================
-- 2) Substitutions
-- ============================================================

type Subst = [SubstEntry]

data SubstEntry where
  (:=) :: HasSpec a => Var a -> Term a -> SubstEntry

backwardsSubstitution :: forall a. HasSpec a => Subst -> Term a -> Term a
backwardsSubstitution sub0 t =
  case findMatch sub0 t of
    -- TODO: what about multiple matches??
    Just x -> V x
    Nothing -> case t of
      Lit a -> Lit a
      V x -> V x
      App f ts -> App f (mapListC @HasSpec (backwardsSubstitution sub0) ts)
  where
    findMatch :: Subst -> Term a -> Maybe (Var a)
    findMatch [] _ = Nothing
    findMatch (x := t' : sub1) t1
      | fastInequality t1 t' = findMatch sub1 t1
      | Just (x', t'') <- cast (x, t')
      , t == t'' =
          Just x'
      | otherwise = findMatch sub1 t1

-- ===================================================================

substituteTerm :: forall a. Subst -> Term a -> Term a
substituteTerm sub = \case
  Lit a -> Lit a
  V x -> substVar sub x
  App f (mapList (substituteTerm sub) -> (ts :: List Term dom)) ->
    case fromLits ts of
      Just vs -> Lit (uncurryList_ unValue (semantics f) vs)
      _ -> App f ts
  where
    substVar :: HasSpec a => Subst -> Var a -> Term a
    substVar [] x = V x
    substVar (y := t : sub1) x
      | Just Refl <- eqVar x y = t
      | otherwise = substVar sub1 x

substituteTerm' :: forall a. Subst -> Term a -> Writer Any (Term a)
substituteTerm' sub = \case
  Lit a -> pure $ Lit a
  V x -> substVar sub x
  App f ts ->
    App f <$> mapMList (substituteTerm' sub) ts
  where
    substVar :: HasSpec a => Subst -> Var a -> Writer Any (Term a)
    substVar [] x = pure $ V x
    substVar (y := t : sub1) x
      | Just Refl <- eqVar x y = t <$ tell (Any True)
      | otherwise = substVar sub1 x

substituteBinder :: HasSpec a => Var a -> Term a -> Binder b -> Binder b
substituteBinder x tm (y :-> p) = y' :-> substitutePred x tm p'
  where
    (y', p') =
      freshen y p (Set.singleton (nameOf x) <> freeVarNames tm <> Set.delete (nameOf y) (freeVarNames p))

substitutePred :: HasSpec a => Var a -> Term a -> Pred -> Pred
substitutePred x tm = \case
  ElemPred bool t xs -> ElemPred bool (substituteTerm [x := tm] t) xs
  GenHint h t -> GenHint h (substituteTerm [x := tm] t)
  Subst x' t p -> substitutePred x tm $ substitutePred x' t p
  Assert t -> Assert (substituteTerm [x := tm] t)
  And ps -> fold (substitutePred x tm <$> ps)
  Exists k b -> Exists (\eval -> k (eval . substituteTerm [x := tm])) (substituteBinder x tm b)
  Let t b -> Let (substituteTerm [x := tm] t) (substituteBinder x tm b)
  ForAll t b -> ForAll (substituteTerm [x := tm] t) (substituteBinder x tm b)
  Case t bs -> Case (substituteTerm [x := tm] t) (mapList (mapWeighted $ substituteBinder x tm) bs)
  When b p -> When (substituteTerm [x := tm] b) (substitutePred x tm p)
  Reifies t' t f -> Reifies (substituteTerm [x := tm] t') (substituteTerm [x := tm] t) f
  DependsOn t t' -> DependsOn (substituteTerm [x := tm] t) (substituteTerm [x := tm] t')
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  Monitor m -> Monitor (\eval -> m (eval . substituteTerm [x := tm]))
  Explain es p -> Explain es $ substitutePred x tm p

-- =====================================================
-- Substituion under an Env, rather than a single Var
-- It takes Values in the Env, and makes them Literals in the Term.

substTerm :: Env -> Term a -> Term a
substTerm env = \case
  Lit a -> Lit a
  V v
    | Just a <- lookupEnv env v -> Lit a
    | otherwise -> V v
  App f (mapList (substTerm env) -> ts) ->
    case fromLits ts of
      Just vs -> Lit (uncurryList_ unValue (semantics f) vs)
      _ -> App f ts

substBinder :: Env -> Binder a -> Binder a
substBinder env (x :-> p) = x :-> substPred (removeVar x env) p

substPred :: Env -> Pred -> Pred
substPred env = \case
  ElemPred bool t xs -> ElemPred bool (substTerm env t) xs
  GenHint h t -> GenHint h (substTerm env t)
  Subst x t p -> substPred env $ substitutePred x t p
  Assert t -> Assert (substTerm env t)
  Reifies t' t f -> Reifies (substTerm env t') (substTerm env t) f
  ForAll set b -> ForAll (substTerm env set) (substBinder env b)
  Case t bs -> Case (substTerm env t) (mapList (mapWeighted $ substBinder env) bs)
  When b p -> When (substTerm env b) (substPred env p)
  DependsOn x y -> DependsOn (substTerm env x) (substTerm env y)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  And ps -> fold (substPred env <$> ps)
  Exists k b -> Exists (\eval -> k $ eval . substTerm env) (substBinder env b)
  Let t b -> Let (substTerm env t) (substBinder env b)
  Monitor m -> Monitor m
  Explain es p -> Explain es $ substPred env p

unBind :: a -> Binder a -> Pred
unBind a (x :-> p) = substPred (singletonEnv x a) p

-- ==========================================================
-- Renaming
-- ============================================================

-- Name

data Name where
  Name :: HasSpec a => Var a -> Name

deriving instance Show Name

instance Eq Name where
  Name v == Name v' = isJust $ eqVar v v'

-- Instances

instance Pretty (Var a) where
  pretty = fromString . show

instance Pretty Name where
  pretty (Name v) = pretty v

instance Ord Name where
  compare (Name v) (Name v') = compare (nameOf v, typeOf v) (nameOf v', typeOf v')

instance Rename Name where
  rename v v' (Name v'') = Name $ rename v v' v''

instance Rename (Term a) where
  rename v v'
    | v == v' = id
    | otherwise = \case
        Lit l -> Lit l
        V v'' -> V (rename v v' v'')
        App f a -> App f (rename v v' a)

instance Rename Pred where
  rename v v'
    | v == v' = id
    | otherwise = \case
        ElemPred bool t xs -> ElemPred bool (rename v v' t) xs
        GenHint h t -> GenHint h (rename v v' t)
        Subst x t p -> rename v v' $ substitutePred x t p
        And ps -> And (rename v v' ps)
        Exists k b -> Exists (\eval -> k $ eval . rename v v') (rename v v' b)
        Let t b -> Let (rename v v' t) (rename v v' b)
        Reifies t' t f -> Reifies (rename v v' t') (rename v v' t) f
        Assert t -> Assert (rename v v' t)
        DependsOn x y -> DependsOn (rename v v' x) (rename v v' y)
        ForAll set b -> ForAll (rename v v' set) (rename v v' b)
        Case t bs -> Case (rename v v' t) (rename v v' bs)
        When b p -> When (rename v v' b) (rename v v' p)
        TruePred -> TruePred
        FalsePred es -> FalsePred es
        Monitor m -> Monitor m
        Explain es p -> Explain es (rename v v' p)

instance Rename (Binder a) where
  rename v v' (va :-> psa) = va' :-> rename v v' psa'
    where
      (va', psa') = freshen va psa (Set.fromList [nameOf v, nameOf v'] <> Set.delete (nameOf va) (freeVarNames psa))

instance Rename (f a) => Rename (Weighted f a) where
  rename v v' (Weighted w t) = Weighted w (rename v v' t)

-- ============================================================================
-- 4) Internals
-- ============================================================================

fromLits :: List Term as -> Maybe (List Value as)
fromLits = mapMList fromLit

fromLit :: Term a -> Maybe (Value a)
fromLit (Lit l) = pure $ Value l
-- fromLit (To x) = (Value . toSimpleRep . unValue) <$> fromLit x -- MAYBE we don't want to do this?
-- fromLit (From x) = (Value . fromSimpleRep . unValue) <$> fromLit x -- Why not apply unary functions to Lit ?
fromLit _ = Nothing

isLit :: Term a -> Bool
isLit = isJust . fromLit

mkCase ::
  HasSpec (SumOver as) => Term (SumOver as) -> List (Weighted Binder) as -> Pred
mkCase tm cs
  | Weighted _ (x :-> p) :> Nil <- cs = Subst x tm p
  -- TODO: all equal maybe?
  | Semigroup.getAll $ foldMapList isTrueBinder cs = TruePred
  | Semigroup.getAll $ foldMapList (isFalseBinder . thing) cs = FalsePred (pure "mkCase on all False")
  | Lit a <- tm = runCaseOn a (mapList thing cs) (\x val p -> substPred (singletonEnv x val) p)
  | otherwise = Case tm cs
  where
    isTrueBinder (Weighted Nothing (_ :-> TruePred)) = Semigroup.All True
    isTrueBinder _ = Semigroup.All False

    isFalseBinder (_ :-> FalsePred {}) = Semigroup.All True
    isFalseBinder _ = Semigroup.All False

runCaseOn ::
  SumOver as ->
  List Binder as ->
  (forall a. (Typeable a, Show a) => Var a -> a -> Pred -> r) ->
  r
runCaseOn _ Nil _ = error "The impossible happened in runCaseOn"
runCaseOn a ((x :-> ps) :> Nil) f = f x a ps
runCaseOn s ((x :-> ps) :> bs@(_ :> _)) f = case s of
  SumLeft a -> f x a ps
  SumRight a -> runCaseOn a bs f

-- ================================================
-- 5) Simple Syntax only transformation of terms.
-- There are other transformation that involve simplifying
-- but they are in module TheKnot
-- ==================================================

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

-- ========================================================================
-- For the API

assertExplain ::
  IsPred p =>
  NE.NonEmpty String ->
  p ->
  Pred
assertExplain nes p = Explain nes (toPred p)

assert ::
  IsPred p =>
  p ->
  Pred
assert p = toPred p

forAll ::
  forall p t a.
  ( Forallable t a
  , HasSpec t
  , HasSpec a
  , IsPred p
  ) =>
  Term t ->
  (Term a -> p) ->
  Pred
forAll tm = mkForAll tm . bind

mkForAll ::
  ( Forallable t a
  , HasSpec t
  , HasSpec a
  ) =>
  Term t ->
  Binder a ->
  Pred
mkForAll (Lit (forAllToList -> [])) _ = TruePred
mkForAll _ (_ :-> TruePred) = TruePred
mkForAll tm binder = ForAll tm binder

exists ::
  forall a p.
  (HasSpec a, IsPred p) =>
  ((forall b. Term b -> b) -> GE a) ->
  (Term a -> p) ->
  Pred
exists sem k =
  Exists sem $ bind k

unsafeExists ::
  forall a p.
  (HasSpec a, IsPred p) =>
  (Term a -> p) ->
  Pred
unsafeExists = exists (\_ -> fatalError "unsafeExists")

letBind ::
  ( HasSpec a
  , IsPred p
  ) =>
  Term a ->
  (Term a -> p) ->
  Pred
letBind tm@V {} body = toPred (body tm)
letBind tm body = Let tm (bind body)

reify ::
  ( HasSpec a
  , HasSpec b
  , IsPred p
  ) =>
  Term a ->
  (a -> b) ->
  (Term b -> p) ->
  Pred
reify t f body =
  exists (\eval -> pure $ f (eval t)) $ \(name "reify_variable" -> x) ->
    [ reifies x t f
    , Explain (pure ("reify " ++ show t ++ " somef $")) $ toPred (body x)
    ]

assertReified :: HasSpec a => Term a -> (a -> Bool) -> Pred
-- Note, it is necessary to introduce the extra variable from the `exists` here
-- to make things like `assertRealMultiple` work, if you don't have it then the
-- `reifies` isn't a defining constraint for anything any more.
assertReified t f =
  reify t f assert

-- | Wrap an 'Explain' around a Pred, unless there is a simpler form.
explanation :: NE.NonEmpty String -> Pred -> Pred
explanation _ p@DependsOn {} = p
explanation _ TruePred = TruePred
explanation es (FalsePred es') = FalsePred (es <> es')
explanation es (Assert t) = Explain es $ Assert t
explanation es p = Explain es p

-- | Add QuickCheck monitoring (e.g. 'Test.QuickCheck.collect' or 'Test.QuickCheck.counterexample')
--   to a predicate. To use the monitoring in a property call 'monitorSpec' on the 'Specification'
--   containing the monitoring and a value generated from the specification.
monitor :: ((forall a. Term a -> a) -> Property -> Property) -> Pred
monitor = Monitor

reifies :: (HasSpec a, HasSpec b) => Term b -> Term a -> (a -> b) -> Pred
reifies = Reifies

dependsOn :: (HasSpec a, HasSpec b) => Term a -> Term b -> Pred
dependsOn = DependsOn

-- lit :: (Typeable a, Eq a, Show a) => a -> Term a
lit :: HasSpec a => a -> Term a
lit = Lit

genHint :: forall t. HasGenHint t => Hint t -> Term t -> Pred
genHint = GenHint . HintF

-- ==============================================================================

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
  When _ pp -> envFromPred env pp
  Subst x a pp -> envFromPred env (substitutePred x a pp)
  Let t (x :-> pp) -> do
    v <- runTerm env t
    envFromPred (extendEnv x v env) pp
  Explain _ pp -> envFromPred env pp
  Exists c (x :-> pp) -> do
    v <- c (errorGE . explain "envFromPred: Exists" . runTerm env)
    envFromPred (extendEnv x v env) pp
  And [] -> pure env
  And (pp : ps) -> do
    env' <- envFromPred env pp
    envFromPred env' (And ps)

-- ==========================================
-- Regularizing

regularize :: HasVariables t => Var a -> t -> Var a
regularize v t =
  case [nameHint v' | Name v' <- Set.toList $ freeVarSet t, nameOf v' == nameOf v, nameHint v' /= "v"] of
    [] -> v
    nh : _ -> v {nameHint = nh}

regularizeBinder :: Binder a -> Binder a
regularizeBinder (x :-> p) = x' :-> substitutePred x (V x') (regularizeNamesPred p)
  where
    x' = regularize x p

regularizeNamesPred :: Pred -> Pred
regularizeNamesPred pred = case pred of
  ElemPred {} -> pred
  Monitor {} -> pred
  And ps -> And $ map regularizeNamesPred ps
  Exists k b -> Exists k (regularizeBinder b)
  Subst v t p -> regularizeNamesPred (substitutePred v t p)
  Let t b -> Let t (regularizeBinder b)
  Assert {} -> pred
  Reifies {} -> pred
  DependsOn {} -> pred
  ForAll t b -> ForAll t (regularizeBinder b)
  Case t bs -> Case t (mapList (mapWeighted regularizeBinder) bs)
  When b p' -> When b (regularizeNamesPred p')
  GenHint {} -> pred
  TruePred {} -> pred
  FalsePred {} -> pred
  Explain es p' -> Explain es (regularizeNamesPred p')

regularizeNames :: Specification a -> Specification a
regularizeNames (ExplainSpec es x) = explainSpec es (regularizeNames x)
regularizeNames (SuspendedSpec x p) =
  SuspendedSpec x' p'
  where
    x' :-> p' = regularizeBinder (x :-> p)
regularizeNames spec = spec

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
computeTermDependencies' = \case
  (App _ args) -> go args
  Lit {} -> (mempty, mempty)
  (V x) -> (noDependencies (Name x), Set.singleton (Name x))
  where
    go :: List Term as -> (DependGraph, Set Name)
    go Nil = (mempty, mempty)
    go (t :> ts) =
      let (gr, ngr) = go ts
          (tgr, ntgr) = computeTermDependencies' t
       in (ntgr `irreflexiveDependencyOn` ngr <> tgr <> gr, ngr <> ntgr)
