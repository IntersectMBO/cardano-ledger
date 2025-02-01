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

{-# OPTIONS_GHC -Wno-orphans #-} -- Rename instances

{-
-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif
-}

-- | This module contains operations and tranformations on Syntax, Term, Pred, etc.
--    1) Computing Free Variables
--    2) Substitution
--    3) Renaming
--    4) internal helper functions
--    5) Syntacic only transformations
module Constrained.SyntaxExperiment where

-- import Constrained.GenericExperiment
import Constrained.WitnessExperiment
import Constrained.BaseExperiment

import Control.Monad.Writer (Writer, tell)
import Data.Typeable
import Constrained.Core(Var(..),eqVar)
import Constrained.Core(Rename(rename),Value(..),unValue,freshen)
import Constrained.GenT(MonadGenError(..),GE(..))
import Constrained.List
import Data.Orphans() -- instances on Symbol
import Constrained.GenericExperiment
import Data.Foldable(toList)
import Data.Maybe(isJust,fromMaybe,isNothing)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.Monoid as Monoid
import Data.Semigroup (Any (..))
import Constrained.Env
import qualified Data.Semigroup as Semigroup
import Data.Foldable(fold)
import Prettyprinter hiding (cat)
import Data.String(fromString)

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

type Subst  = [SubstEntry]

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

-- | Sound but not complete inequality on terms
fastInequality :: Term a -> Term b -> Bool
fastInequality (V (Var i _)) (V (Var j _)) = i /= j
fastInequality Lit {} Lit {} = False
fastInequality (App _ as) (App _ bs) = go as bs
  where
    go :: List Term as -> List Term bs -> Bool
    go Nil Nil = False
    go (a :> as') (b :> bs') = fastInequality a b || go as' bs'
    go _ _ = True
fastInequality _ _ = True

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

substPred :: Env -> Pred  -> Pred 
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

-- | This extracts the semantics of a witness (i.e. a function over Terms) 
--   Recall FunctionSymbols are functions that you can use when writing Terms
--   Usually the Haskel name ends in '_', i.e. not_, subset_ ,lookup_
--   And infix FunctionSymbols names end in '.', ie. ==. , <=. etc.
--   E.g  app NotW :: Term Bool -> Term Bool
--        app NotW (lit False)  ==reducesto==> not_ False
--   this functionality is embedded in the Haskel function not_
--   Note the witness (NotW) must have a FunctionSymbol instance like
--   instance FunctionSymbol "not_" BaseWitness '[Bool] Bool where ...
--             Name in Haskell^      ^  its arguments^   ^ its result
--                  The type of NotW |
app ::
  (FunctionSymbol c sym wit as b ) =>
  wit c sym as b ->
  FunTy (MapList Term as) (Term b)
app fn = curryList (App fn)

fromLits :: List Term as -> Maybe (List Value as)
fromLits = mapMList fromLit

fromLit :: Term a -> Maybe (Value a)
fromLit (Lit l) = pure $ Value l
fromLit _ = Nothing

isLit :: Term a -> Bool
isLit = isJust . fromLit
 
name :: String -> Term a -> Term a
name nh (V (Var i _)) = V (Var i nh)
name _ _ = error "applying name to non-var thing! Shame on you!"

-- | Give a Term a nameHint, if its a Var, and doesn't already have one,
--  otherwise return the Term unchanged.
named :: String -> Term a -> Term a
named nh t@(V (Var i x)) = if x /= "v" then t else V (Var i nh)
named _ t = t

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
  (forall a. HasSpec a => Var a -> a -> Pred -> r) ->
  r
runCaseOn _ Nil _ = error "The impossible happened in runCaseOn"
runCaseOn a ((x :-> ps) :> Nil) f = f x a ps
runCaseOn s ((x :-> ps) :> bs@(_ :> _)) f = case s of
  SumLeft a -> f x a ps
  SumRight a -> runCaseOn a bs f

-- ================================================
-- 5) Simple Syntax only transformation of terms.
-- There are other transformation that involve simplifying
-- but they are in the SimplifyExperiment module
-- ==================================================

-- Common subexpression elimination but only on terms that are already let-bound.
letSubexpressionElimination :: HasSpec Bool => Pred -> Pred
letSubexpressionElimination = go []
  where
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
      [Pred ] ->
      (Binder a -> Pred ) ->
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
        explainSemantics k2 env = explain es $ k2 env
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

