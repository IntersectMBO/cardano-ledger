{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- Base types: Term, Pred, Spec, Ctx, and classes: HasSpec, Syntax, Semantics, and Logic for the MinModel
module MinBase where

-- import qualified Constrained.Base as Base
import Constrained.Core (
  Evidence (..),
  Rename (rename),
  Value (..),
  Var (..),
  eqVar,
  freshen,
  unValue,
  unionWithMaybe,
 )
import Constrained.Env
import Constrained.GenT
import Constrained.Graph
import Constrained.List hiding (ListCtx)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable (fold)
import qualified Data.Foldable as Foldable (fold, toList)
import Data.Kind
import Data.List (intersect, nub, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import qualified Data.Monoid as Monoid
import Data.Semigroup (Any (..), Max (..), getMax, sconcat)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Typeable
import GHC.Stack
import Prettyprinter
import Test.QuickCheck hiding (forAll)

-- ===========================================
-- Terms
-- ===========================================

type AppRequires t dom rng =
  ( Logic t
  , TypeList dom
  , Eq (t dom rng)
  , Show (t dom rng)
  , Typeable dom
  , Typeable rng
  , All HasSpec dom
  , HasSpec rng
  )

data Term a where
  App ::
    forall t dom rng.
    AppRequires t dom rng =>
    t dom rng ->
    List Term dom ->
    Term rng
  Lit :: (Typeable a, Eq a, Show a) => a -> Term a
  V :: HasSpec a => Var a -> Term a

instance Eq (Term a) where
  V x == V x' = x == x'
  Lit a == Lit b = a == b
  App (w1 :: x1) (ts :: List Term dom1) == App (w2 :: x2) (ts' :: List Term dom2) =
    case (eqT @dom1 @dom2, eqT @x1 @x2) of
      (Just Refl, Just Refl) ->
        w1 == w2
          && sameTerms ts ts'
      _ -> False
  _ == _ = False

-- How to compare the args of two applications for equality
sameTerms :: All HasSpec as => List Term as -> List Term as -> Bool
sameTerms Nil Nil = True
sameTerms (x :> xs) (y :> ys) = x == y && sameTerms xs ys

-- ===========================================
-- Function Symbol Classes
-- ===========================================

-- | Syntactic operations are ones that have to do with the structure and appearence of the type.
class Syntax (t :: [Type] -> Type -> Type) where
  inFix :: forall dom rng. t dom rng -> Bool
  inFix _ = False
  name :: forall dom rng. t dom rng -> String

-- | Semantic operations are ones that give the function symbol, meaning as a function.
--   I.e. how to apply the function to a list of arguments and return a value.
class Syntax t => Semantics (t :: [Type] -> Type -> Type) where
  semantics :: forall d r. t d r -> FunTy d r -- e.g. FunTy '[a,Int] Bool == a -> Int -> Bool

-- | Logical operations are one that support reasoning about how a function symbol
--   relates to logical properties, that we call Spec's
class (Typeable t, Syntax t, Semantics t) => Logic (t :: [Type] -> Type -> Type) where
  {-# MINIMAL propagate | (propagateTypeSpec, propagateMemberSpec) #-}

  propagateTypeSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as a -> TypeSpec b -> [b] -> Spec a
  propagateTypeSpec f ctx ts cant = propagate f ctx (TypeSpec ts cant)

  propagateMemberSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as a -> NonEmpty b -> Spec a
  propagateMemberSpec f ctx xs = propagate f ctx (MemberSpec xs)

  propagate ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as a -> Spec b -> Spec a
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec es) = ErrorSpec es
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  propagate f ctx (TypeSpec ts cant) = propagateTypeSpec f ctx ts cant
  propagate f ctx (MemberSpec xs) = propagateMemberSpec f ctx xs

-- ===========================
-- Pred
-- ===========================

data Pred where
  ElemPred :: forall a. HasSpec a => Bool -> Term a -> NonEmpty a -> Pred
  And :: [Pred] -> Pred
  ForAll :: (Forallable t a, HasSpec t, HasSpec a) => Term t -> Binder a -> Pred
  Assert :: Term Bool -> Pred
  TruePred :: Pred
  FalsePred :: NonEmpty String -> Pred
  Case :: HasSpec (Either a b) => Term (Either a b) -> Binder a -> Binder b -> Pred
  Let :: Term a -> Binder a -> Pred
  Subst :: HasSpec a => Var a -> Term a -> Pred -> Pred

data Binder a where
  (:->) :: HasSpec a => Var a -> Pred -> Binder a

class Forallable t e | t -> e where
  fromForAllSpec :: (HasSpec t, HasSpec e) => Spec e -> Spec t
  forAllToList :: t -> [e]

-- ================================
-- Spec
-- ================================

data Spec a where
  TrueSpec :: Spec a
  ErrorSpec :: NonEmpty String -> Spec a
  SuspendedSpec :: HasSpec a => Var a -> Pred -> Spec a -- Maybe we elide this at first
  MemberSpec :: NonEmpty a -> Spec a
  TypeSpec :: HasSpec a => TypeSpec a -> [a] -> Spec a

typeSpec :: HasSpec a => TypeSpec a -> Spec a
typeSpec ts = TypeSpec ts mempty

constrained :: forall a. HasSpec a => (Term a -> Pred) -> Spec a
constrained body =
  let x :-> p = bind body
   in SuspendedSpec x p

bind :: HasSpec a => (Term a -> Pred) -> Binder a
bind bodyf = newv :-> bodyPred
  where
    bodyPred = {- toPred -} body
    newv = Var (nextVar bodyPred) "v"
    body = bodyf (V newv)

    nextVar q = 1 + bound q

    boundBinder :: Binder a -> Int
    boundBinder (x :-> p) = max (nameOf x) (bound p)

    bound (ElemPred _ _ _) = -1
    bound (Subst x _ p) = max (nameOf x) (bound p)
    bound (And ps) = maximum $ (-1) : map bound ps -- (-1) as the default to get 0 as `nextVar p`
    bound (Let _ b) = boundBinder b
    bound (ForAll _ b) = boundBinder b
    bound (Case _ ba bb) = max (boundBinder ba) (boundBinder bb)
    bound Assert {} = -1
    bound TruePred = -1
    bound FalsePred {} = -1

-- ========================================
-- HasSpec
-- ========================================

class (Typeable a, Eq a, Show a, Show (TypeSpec a), Typeable (TypeSpec a)) => HasSpec a where
  -- | The `TypeSpec a` is the type-specific `Spec a`.
  type TypeSpec a

  -- `TypeSpec` behaves sort-of like a monoid with a neutral
  -- element `emptySpec` and a `combineSpec` for combining
  -- two `TypeSpec a`. However, in order to provide flexibilty
  -- `combineSpec` takes two `TypeSpec` and constucts a `Spec`. This
  -- avoids e.g. having to have a separate implementation of `ErrorSpec`
  -- and `MemberSpec` in `TypeSpec`.

  emptySpec :: TypeSpec a
  combineSpec :: TypeSpec a -> TypeSpec a -> Spec a

  -- | Generate a value that satisfies the `TypeSpec`.
  -- The key property for this generator is soundness:
  --  ∀ a ∈ genFromTypeSpec spec. a `conformsTo` spec
  genFromTypeSpec :: (HasCallStack, MonadGenError m) => TypeSpec a -> GenT m a

  -- | Check conformance to the spec.
  conformsTo :: HasCallStack => a -> TypeSpec a -> Bool

  -- | Convert a spec to predicates:
  -- The key property here is:
  --   ∀ a. a `conformsTo` spec == a `conformsTo` constrained (\t -> toPreds t spec)
  toPreds :: Term a -> TypeSpec a -> Pred

  -- | This is used to detect self inconsistencies in a (TypeSpec t)
  --   guardTypeSpec message ty --> ErrorSpec message, if ty is inconsistent
  guardTypeSpec :: TypeSpec a -> Spec a
  guardTypeSpec ty = typeSpec ty

-- =========================================
-- Contexts
-- =========================================

data HOLE a where HOLE :: HOLE a

data ListCtx (as :: [Type]) a where
  Unary :: Ctx a a -> ListCtx '[a] a
  (:-+) :: Ctx b a -> b -> ListCtx '[a, b] a
  (:+-) :: a -> Ctx a b -> ListCtx '[a, b] b

data Ctx var (hole :: Type) where
  CtxApp ::
    ( AppRequires fn as b
    , HasSpec b
    , TypeList as
    , Typeable as
    , All HasSpec as
    , Logic fn
    ) =>
    fn as b -> ListCtx as v -> Ctx v b
  Hole :: HasSpec v => Ctx v v

ctxHasSpec :: Ctx v a -> Evidence (HasSpec a)
ctxHasSpec Hole = Evidence
ctxHasSpec CtxApp {} = Evidence

-- | From a ListCtx, build a (List Term as), to which the function symbol can be applied.
--   Hole becomes 't', values become `Lit`
fromListCtx :: All HasSpec as => ListCtx as a -> Term a -> List Term as
fromListCtx (Unary c) t = t :> Nil
fromListCtx (Hole :-+ y) t = t :> Lit y :> Nil
fromListCtx (x :+- Hole) t = Lit x :> t :> Nil
