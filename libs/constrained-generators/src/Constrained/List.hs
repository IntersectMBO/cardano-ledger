{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.List where

import Data.Functor.Const
import Data.Kind
import Data.Semigroup (Sum (..))

-- | A heterogeneous list / an inductive tuple.
-- We use this heavily to represent arguments to
-- functions in terms. A term application (c.f. `Constrained.Base`)
-- is represented as `App :: fn as b -> List (Term fn) as -> Term fn b`
-- for example.
data List (f :: k -> Type) (as :: [k]) where
  Nil :: List f '[]
  (:>) :: f a -> List f as -> List f (a : as)

infixr 5 :>

deriving instance (forall a. Show (f a)) => Show (List f as)
deriving instance (forall a. Eq (f a)) => Eq (List f as)

mapList :: (forall a. f a -> g a) -> List f as -> List g as
mapList _ Nil = Nil
mapList f (x :> xs) = f x :> mapList f xs

mapListC :: forall c as f g. All c as => (forall a. c a => f a -> g a) -> List f as -> List g as
mapListC _ Nil = Nil
mapListC f (x :> xs) = f x :> mapListC @c f xs

mapMList :: Applicative m => (forall a. f a -> m (g a)) -> List f as -> m (List g as)
mapMList _ Nil = pure Nil
mapMList f (x :> xs) = (:>) <$> f x <*> mapMList f xs

mapMListC ::
  forall c as f g m.
  (Applicative m, All c as) =>
  (forall a. c a => f a -> m (g a)) ->
  List f as ->
  m (List g as)
mapMListC _ Nil = pure Nil
mapMListC f (x :> xs) = (:>) <$> f x <*> mapMListC @c f xs

foldMapList :: Monoid b => (forall a. f a -> b) -> List f as -> b
foldMapList _ Nil = mempty
foldMapList f (a :> as) = f a <> foldMapList f as

foldMapListC ::
  forall c as b f. (All c as, Monoid b) => (forall a. c a => f a -> b) -> List f as -> b
foldMapListC _ Nil = mempty
foldMapListC f (a :> as) = f a <> foldMapListC @c f as

appendList :: List f as -> List f bs -> List f (Append as bs)
appendList Nil bs = bs
appendList (a :> as) bs = a :> appendList as bs

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

-- | A List with a hole in it, can be seen as a zipper
-- over type-level lists.
--
-- We use this to represent contexts over terms in `Constrained.Base`.
-- For example, an application of `f` to a single variable of type `a`
-- would be the pair `(fn as b, ListCtx Value as (HOLE a))` where
-- `HOLE` (c.f. `Constrained.Base`) is isomorphic to `:~:`.
data ListCtx f (as :: [Type]) c where
  (:?) :: c a -> List f as -> ListCtx f (a : as) c
  (:!) :: f a -> ListCtx f as c -> ListCtx f (a : as) c

infixr 5 :?, :!

-- | A Convenient pattern for singleton contexts
pattern NilCtx :: c a -> ListCtx f '[a] c
pattern NilCtx c = ListCtx Nil c Nil

{-# COMPLETE NilCtx #-}

-- | A view of a `ListCtx` where you see the whole context at the same time.
pattern ListCtx ::
  () => as'' ~ Append as (a : as') => List f as -> c a -> List f as' -> ListCtx f as'' c
pattern ListCtx as c as' <- (toWholeCtx -> ListCtxWhole as c as')
  where
    ListCtx as c as' = fromWholeCtx $ ListCtxWhole as c as'

{-# COMPLETE ListCtx #-}

-- | Internals for the `ListCtx` pattern
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

fillListCtx :: ListCtx f as c -> (forall a. c a -> f a) -> List f as
fillListCtx (ListCtx pre c post) f = appendList pre (f c :> post)

mapListCtx :: (forall a. f a -> g a) -> ListCtx f as c -> ListCtx g as c
mapListCtx nt (ListCtx pre c post) = ListCtx (mapList nt pre) c (mapList nt post)

mapListCtxC ::
  forall c as f g h. All c as => (forall a. c a => f a -> g a) -> ListCtx f as h -> ListCtx g as h
mapListCtxC nt (h :? as) = h :? mapListC @c nt as
mapListCtxC nt (a :! ctx) = nt a :! mapListCtxC @c nt ctx

-- | A function type from `ts` to `res`:
--    FunTy '[Int, Bool] Double = Int -> Bool -> Double
type family FunTy ts res where
  FunTy '[] a = a
  FunTy (a : as) r = a -> FunTy as r

-- | Materialize the shape of the type list `as`, this is very useful
-- for avoiding having to write type classes that recurse over `as`.
listShape :: forall as. TypeList as => List (Const ()) as
listShape = unfoldList (\_ -> Const ())

-- | Higher-order functions for working on `List`s
class TypeList ts where
  uncurryList :: FunTy (MapList f ts) r -> List f ts -> r
  uncurryList_ :: (forall a. f a -> a) -> FunTy ts r -> List f ts -> r
  curryList :: (List f ts -> r) -> FunTy (MapList f ts) r
  curryList_ :: (forall a. a -> f a) -> (List f ts -> r) -> FunTy ts r
  unfoldList :: (forall a as. List f as -> f a) -> List f ts

-- | NOTE: the two instances for `TypeList` are `TypeList '[]` and
-- `TypeList '(a : as)`. That way its basically just a structurally
-- recursive function on type-level lists that computes the `TypeList`
-- dictionary.
instance TypeList '[] where
  uncurryList a Nil = a
  uncurryList_ _ a Nil = a
  curryList f = f Nil
  curryList_ _ f = f Nil
  unfoldList _ = Nil

instance TypeList as => TypeList (a : as) where
  uncurryList f (a :> as) = uncurryList (f a) as
  uncurryList_ k f (a :> as) = uncurryList_ k (f $ k a) as
  curryList f a = curryList (\xs -> f (a :> xs))
  curryList_ p f a = curryList_ p (\xs -> f (p a :> xs))
  unfoldList f = let xs = unfoldList f in f xs :> xs

type family All (c :: k -> Constraint) (as :: [k]) :: Constraint where
  All c '[] = ()
  All c (a : as) = (c a, All c as)
