{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The idea is to create a world of Haskell types, called a Universe. Then to
--   define a 'class' whose methods are only applicable to that set of types. We define
--   a closed set by defining an indexed type constructor Rep. If (Singleton Rep), then it must
--   be that every value of type (Rep t) has exactly 0 or 1 inhabitants. (Rep t) has exactly
--   1 inhabitant if 't' is in the universe, and 0 inhabitants if it is not in the universe.
--   A closely related class is (Indexed s t). We would like a total ordering on (t i) and (t j),
--   where the indices, i and j, are different. We do this by ensuring every value of type (t i)
--   has a unique Shape, and that the type Shape has a total order. Every closed singleton can
--   be totally ordered this way. Other indexed types are used to make well-type terms. I.e.
--   Indexed s t => Exp t, denotes a Haskell data type Exp, related to the type t.
--   Note, unlike singletons, there may not be a unique value of type (Exp t), but there may be
--   a unique Shape for every value.
module Data.Universe (
  Shaped (..),
  Singleton (..),
  Universe (..),
  Any (..),
  Some (..),
  Dyn (..),
  Shape (..),
  Eql,
  (:~:) (Refl),
  TypeRep,
  compareByShape,
)
where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (Refl))
#if __GLASGOW_HASKELL__ < 906
import Type.Reflection (TypeRep, pattern App, pattern Con, pattern Fun)
#else
-- Ghc-9.6 removed the Fun constructor.
import Type.Reflection (TypeRep, pattern App, pattern Con)
#endif

-- ==================================================

-- | Type synonym, so we can use ( :~: ) without TypeOperators
type Eql x y = x :~: y

-- | Given (Singleton T), a value of type (T i) has exactly 0 or 1 inhabitants,
--   so we can compare the structure of the type to get proofs that the indexes (i and j)
--   are the same type, using testEql, at runtime. 'cmpIndex', it is like 'compare'
--   except we can have two different indexes ('a' and 'b').
class Singleton t where
  testEql :: t i -> t j -> Maybe (i :~: j)
  cmpIndex :: t a -> t b -> Ordering

-- | For some indexed types, we can assign a unique Shape to each type,
--   using: 'shape' :: t i -> Shape s   (where s is some Singleton type)
--   the class (Shaped t s) means we can assign a unique Shape to each
--   value of type (t i). Not quite as strong as having a unique inhabitant.
--   Every closed singleton type can be shaped, and many other indexed types
--   can be shaped as well.
class Shaped t rep where
  shape :: t i -> Shape rep

-- | Order two indexed types by their Shape. This is usefull for making (Singleton t)
--   instances when we have (Shaped t r) instances, as cmpIndex can be compareByShape.
compareByShape ::
  forall (rep :: Type -> Type) (t :: Type -> Type) (i :: Type) (j :: Type).
  Shaped t rep =>
  t i ->
  t j ->
  Ordering
compareByShape x y = compare (shape @t @rep x) (shape @t @rep y)

-- | A minimal class of operations defined on the universe described by 'rep' .
--   Feel free to make your own Universe classes with additional methods
class Singleton rep => Universe rep t where
  repOf :: rep t

-- ======================================================================
-- A number of types that existentially quantify (i.e. hide) a type index
-- Each one adding more detail, so that the types can do more

-- | Hide the index for any indexed type 't'
data Any t where
  Any :: t i -> Any t

deriving instance (forall i. Show (t i)) => Show (Any t)

-- | Hide the index for a singleton type 't'
data Some t where
  Some :: Singleton t => t i -> Some t

data Dyn rep where
  Dyn :: Singleton t => t i -> i -> Dyn t

-- ==============================================================================

-- A Shape is a mechanism to define Ord instances for (Some R) where R is an indexed type.
-- Let R be an indexed type, so we have values of types like (R Int), (R [Bool]) etc.  We
-- exploit the fact that some indexed types have a unique Shape for every value of type (R i).
-- We assign this unique Shape using :  shape:: R t -> Shape x.
data Shape rep where
  Nullary :: Int -> Shape rep
  Nary :: Int -> [Shape rep] -> Shape rep
  Esc :: (Singleton rep, Ord t) => rep t -> t -> Shape rep

instance Ord (Shape rep) where
  compare (Nullary i) (Nullary j) = compare i j
  compare (Nullary _) _ = LT
  compare _ (Nullary _) = GT
  compare (Nary n xs) (Nary m ys) = compare (n, xs) (m, ys)
  compare (Nary _ _) _ = LT
  compare _ (Nary _ _) = GT
  compare (Esc r1 t1) (Esc r2 t2) =
    case testEql r1 r2 of
      Just Refl -> compare t1 t2
      Nothing -> cmpIndex r1 r2

instance Eq (Shape rep) where
  a == b = compare a b == EQ

-- =============================================================
-- Every Singleton type 'r' has Eq(Some r) and Ord(Some r) instances

instance Ord (Some r) where
  compare (Some x) (Some y) = cmpIndex x y

instance Eq (Some r) where
  (Some x) == (Some y) = cmpIndex x y == EQ

-- A strategy for defining Eq and Ord instances for (Any t) if it is Shaped
-- We don't actually make these instances, because there are other strategies
-- we don't want to preclude. But just replace 't' with an indexed type 'R'
-- and supply the (Shaped R TypeRep) instance. Actually any concrete singleton
-- type Rep, can be used in place of TypeRep, as well.
{-
instance Shaped t TypeRep => Eq (Any t) where
  (Any x) == (Any y) = compare @(Shape _) (shape @t @TypeRep x) (shape @t @TypeRep y) == EQ

instance Shaped t TypeRep => Ord (Any t) where
  compare (Any x) (Any y) = compare @(Shape _) (shape @t @TypeRep x) (shape @t @TypeRep y)
-}

-- =======================================================
{- Now we have two ways to make Ord instances for (Some R).
1) if R has a (Singleton R) instance we write

instance Singleton t => Ord (Some t) where
  compare (Some x) (Some y) = cmpIndex @t x y
instance Singleton t => Eq (Some t) where
  x == y = compare x y == EQ

2) if R has a (Shaped R rep) instance we write

instance (forall rep . Shaped t rep) => Ord (Some t) where
  compare (Some x) (Some y) = compare (shape @t x) (shape @t y)
instance (forall rep. Shaped t rep) => Eq (Some t) where
  x == y = compare x y == EQ
-}

-- =============================================================
-- An open Singleton type from Type.Reflection(TypeRep)

compareTypeRep :: TypeRep a -> TypeRep b -> Ordering
compareTypeRep (Con x) (Con y) = compare x y
compareTypeRep (Con _) _ = LT
compareTypeRep _ (Con _) = GT
compareTypeRep (App x a) (App y b) =
  case compareTypeRep x y of
    EQ -> compareTypeRep a b
    other -> other
#if __GLASGOW_HASKELL__ < 906
-- Ghc-9.6 removed the Fun constructor making these redundant.
compareTypeRep (App _ _) _ = LT
compareTypeRep _ (App _ _) = GT
compareTypeRep (Fun x a) (Fun y b) =
  case compareTypeRep x y of
    EQ -> compareTypeRep a b
    other -> other
#endif

instance Singleton TypeRep where
  testEql = testEquality
  cmpIndex = compareTypeRep

-- =============================================================
-- A very simple closed Singleton type

data R t where
  IntR :: R Int
  BoolR :: R Bool

instance Shaped R any where
  shape IntR = Nullary 0
  shape BoolR = Nullary 1

instance Singleton R where
  testEql IntR IntR = Just Refl
  testEql BoolR BoolR = Just Refl
  testEql _ _ = Nothing
  cmpIndex x y = compare @(Shape TypeRep) (shape @R x) (shape @R y)
