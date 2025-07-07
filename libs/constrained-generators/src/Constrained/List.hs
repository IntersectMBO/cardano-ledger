{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | A module for working with type-indexed heterogenous lists, sometimes
-- called inductive tuples.
module Constrained.List (
  -- * Lists
  List (..),

  -- ** Type families
  Length,
  (:!),
  All,
  MapList,
  Append,
  FunTy,

  -- ** Common functions for working with `List`
  TypeList (..),
  toList,
  toListC,
  mapList,
  mapListC,
  mapMList,
  mapMListC,
  foldMapList,
  foldMapListC,
  appendList,
  lengthList,
  uncurryList,
  uncurryList_,

  -- * List contexts
  ListCtx (..),
  pattern NilCtx,
  pattern ListCtx,
  fillListCtx,
  mapListCtx,
  mapListCtxC,
) where

import Data.Foldable (fold)
import Data.Functor.Const
import Data.Kind
import Data.Semigroup (Sum (..))
import GHC.TypeLits

-- | A heterogeneous list / an inductive tuple. We use this heavily to
-- represent arguments to functions in terms
data List (f :: k -> Type) (as :: [k]) where
  Nil :: List f '[]
  (:>) :: f a -> List f as -> List f (a : as)

infixr 5 :>

deriving instance (forall a. Show (f a)) => Show (List f as)

deriving instance (forall a. Eq (f a)) => Eq (List f as)

-- | Type level `length`
type family Length (as :: [k]) :: Nat where
  Length '[] = 0
  Length (_ : as) = 1 + Length as

-- | Get the @n@:th element of the type-level list @as@
type family (as :: [k]) :! n :: k where
  '[] :! n = TypeError ('Text "Indexing into empty type-level list")
  (a : as) :! 0 = a
  (a : as) :! n = as :! (n - 1)

-- | Convert a @`List` f@ to a normal list with an algebra for @f@
toList :: (forall a. f a -> b) -> List f as -> [b]
toList _ Nil = []
toList f (x :> xs) = f x : toList f xs

-- | Like `toList` when you need a constraint on the elements of the index of the `List`
toListC :: forall c f as b. All c as => (forall a. c a => f a -> b) -> List f as -> [b]
toListC _ Nil = []
toListC f (x :> xs) = f x : toListC @c f xs

-- | Map a natural transformation from @f@ to @g@ over a `List`
mapList :: (forall a. f a -> g a) -> List f as -> List g as
mapList _ Nil = Nil
mapList f (x :> xs) = f x :> mapList f xs

-- | Like `mapList` where the natural transformation is constrained
mapListC :: forall c as f g. All c as => (forall a. c a => f a -> g a) -> List f as -> List g as
mapListC _ Nil = Nil
mapListC f (x :> xs) = f x :> mapListC @c f xs

-- | Monadic (actually applicative) `mapList`
mapMList :: Applicative m => (forall a. f a -> m (g a)) -> List f as -> m (List g as)
mapMList _ Nil = pure Nil
mapMList f (x :> xs) = (:>) <$> f x <*> mapMList f xs

-- | Monadic (actually applicative) `mapListC`
mapMListC ::
  forall c as f g m.
  (Applicative m, All c as) =>
  (forall a. c a => f a -> m (g a)) ->
  List f as ->
  m (List g as)
mapMListC _ Nil = pure Nil
mapMListC f (x :> xs) = (:>) <$> f x <*> mapMListC @c f xs

-- | Like `foldMap` for t`List`
foldMapList :: Monoid b => (forall a. f a -> b) -> List f as -> b
foldMapList f = fold . toList f

-- | Like `foldMapList` where the mapped function has a constraint
foldMapListC ::
  forall c as b f. (All c as, Monoid b) => (forall a. c a => f a -> b) -> List f as -> b
foldMapListC f = fold . toListC @c f

-- | Append two t`List`s
appendList :: List f as -> List f bs -> List f (Append as bs)
appendList Nil bs = bs
appendList (a :> as) bs = a :> appendList as bs

-- | Like `length` for `List`
lengthList :: List f as -> Int
lengthList = getSum . foldMapList (const $ Sum 1)

-- | Append two type-level lists
type family Append as as' where
  Append '[] as' = as'
  Append (a : as) as' = a : Append as as'

-- | Map a type functor over a list
type family MapList (f :: k -> j) as where
  MapList f '[] = '[]
  MapList f (a : as) = f a : MapList f as

-- | A function type from @ts@ to @res@:
--  @FunTy '[Int, Bool] Double = Int -> Bool -> Double@
type family FunTy ts res where
  FunTy '[] a = a
  FunTy (a : as) r = a -> FunTy as r

-- | Apply a function that takes @`MapList` f ts@ to a @`List` f ts@
uncurryList :: FunTy (MapList f ts) r -> List f ts -> r
uncurryList r Nil = r
uncurryList f (a :> as) = uncurryList (f a) as

-- | Like `uncurryList` but first apply an algebra to get rid of the @f@ type
-- wrapper
uncurryList_ :: (forall a. f a -> a) -> FunTy ts r -> List f ts -> r
uncurryList_ _ a Nil = a
uncurryList_ k f (a :> as) = uncurryList_ k (f $ k a) as

-- | Higher-order functions for working on `List`s
class TypeList ts where
  curryList :: (List f ts -> r) -> FunTy (MapList f ts) r
  curryList_ :: (forall a. a -> f a) -> (List f ts -> r) -> FunTy ts r

  -- | Materialize the shape of the type list @as@, this is very useful
  -- for avoiding having to write type classes that recurse over @as@.
  listShape :: List (Const ()) ts

-- | NOTE: the two instances for `TypeList` are @`TypeList` []@ and
-- @`TypeList` (a : as)@. That way its basically just a structurally
-- recursive function on type-level lists that computes the `TypeList`
-- dictionary (mostly) statically.
instance TypeList '[] where
  curryList f = f Nil
  curryList_ _ f = f Nil
  listShape = Nil

instance TypeList as => TypeList (a : as) where
  curryList f a = curryList (\xs -> f (a :> xs))
  curryList_ p f a = curryList_ p (\xs -> f (p a :> xs))
  listShape = Const () :> listShape

-- | Every element @a@ of @as@ obeys constraint @c a@
type family All (c :: k -> Constraint) (as :: [k]) :: Constraint where
  All c '[] = ()
  All c (a : as) = (c a, All c as)

-- | A List with a hole in it, can be seen as a zipper
-- over type-level lists.
--
-- We use this to represent arguments to functions in
-- evaluation contexts (terms with a single hole).
data ListCtx f (as :: [Type]) c where
  (:?) :: c a -> List f as -> ListCtx f (a : as) c
  (:!) :: f a -> ListCtx f as c -> ListCtx f (a : as) c

infixr 5 :?, :!

-- | A Convenient pattern for singleton contexts
pattern NilCtx :: c a -> ListCtx f '[a] c
pattern NilCtx c = ListCtx Nil c Nil

{-# COMPLETE NilCtx #-}

-- | A view of a t`ListCtx` where you see the whole context at the same time.
pattern ListCtx ::
  () => as'' ~ Append as (a : as') => List f as -> c a -> List f as' -> ListCtx f as'' c
pattern ListCtx as c as' <- (toWholeCtx -> ListCtxWhole as c as')
  where
    ListCtx as c as' = fromWholeCtx $ ListCtxWhole as c as'

{-# COMPLETE ListCtx #-}

-- | Internals for the t`ListCtx` pattern
data ListCtxWhole f as c where
  ListCtxWhole ::
    List f as ->
    c a ->
    List f as' ->
    ListCtxWhole f (Append as (a : as')) c

toWholeCtx :: ListCtx f as c -> ListCtxWhole f as c
toWholeCtx (hole :? suf) = ListCtxWhole Nil hole suf
toWholeCtx (x :! xs)
  | ListCtxWhole pre hole suf <- toWholeCtx xs =
      ListCtxWhole (x :> pre) hole suf

fromWholeCtx :: ListCtxWhole f as c -> ListCtx f as c
fromWholeCtx (ListCtxWhole Nil hole suf) = hole :? suf
fromWholeCtx (ListCtxWhole (x :> pre) hole suf) = x :! fromWholeCtx (ListCtxWhole pre hole suf)

-- | Instantiate the hole in a t`ListCtx` to obtain a t`List`
fillListCtx :: ListCtx f as c -> (forall a. c a -> f a) -> List f as
fillListCtx (ListCtx pre c post) f = appendList pre (f c :> post)

-- | Transform a @t`ListCtx` f c@ to a @t`ListCtx` g` c@ via a natural transformation
mapListCtx :: (forall a. f a -> g a) -> ListCtx f as c -> ListCtx g as c
mapListCtx nt (ListCtx pre c post) = ListCtx (mapList nt pre) c (mapList nt post)

-- | Like `mapListCtx` but the natural transformation may have a constraint
mapListCtxC ::
  forall c as f g h. All c as => (forall a. c a => f a -> g a) -> ListCtx f as h -> ListCtx g as h
mapListCtxC nt (h :? as) = h :? mapListC @c nt as
mapListCtxC nt (a :! ctx) = nt a :! mapListCtxC @c nt ctx
