{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.Univ where

import Constrained.List
import Control.Monad.Identity
import Data.Functor.Const
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

class FunctionLike fn where
  -- | The semantics of a function is given by `sem`
  sem :: fn as b -> FunTy as b

-- | Evidence that `fn` exists somewhere in `fnU`. To make it possible
-- for `fnU` to consist of a balanced tree of functions we need the `path`
-- to help GHC disambiguate instances of this class during solving.
class IsMember (fn :: [Type] -> Type -> Type) fnU (path :: [PathElem]) where
  -- | Inject a specific function symbol into a function
  -- universe.
  injectFn0 :: fn as b -> fnU as b

  extractFn0 :: fnU as b -> Maybe (fn as b)

type Member fn univ = IsMember fn univ (Path fn univ)

injectFn :: forall fn fnU as b. Member fn fnU => fn as b -> fnU as b
injectFn = injectFn0 @fn @fnU @(Path fn fnU)

extractFn :: forall fn fnU as b. Member fn fnU => fnU as b -> Maybe (fn as b)
extractFn = extractFn0 @fn @fnU @(Path fn fnU)

data PathElem = PFix | PLeft | PRight

------------------------------------------------------------------------
-- Fixpoints
------------------------------------------------------------------------

{-
On the most basic level what we are after is a first order representation of
typed functions. Furthermore, we need the representation to be compositional so
that you can plug-in your own functions into the universe.

That might sound like it's no problem, but the issue is with higher-order
functions. Consider:

data ListFn where
  MapFn :: _ '[a] b -> ListFn '[[a]] [b]

What should go in place of `_`? It turns out that you need to specify the global
function universe and make sure the global universe is the same everywhere.

data ListFn fn where
  MapFn :: fn '[a] b -> ListFn fn '[[a]] [b]

-}

newtype Fix (fn :: Univ -> Univ) (as :: [Type]) b = Fix (fn (Fix fn) as b)

deriving via (fn (Fix fn)) instance FunctionLike (fn (Fix fn)) => FunctionLike (Fix fn)
deriving via (fn (Fix fn) as b) instance Eq (fn (Fix fn) as b) => Eq (Fix fn as b)
deriving via (fn (Fix fn) as b) instance Show (fn (Fix fn) as b) => Show (Fix fn as b)

------------------------------------------------------------------------
-- Universes
------------------------------------------------------------------------

type Univ = [Type] -> Type -> Type

-- TODO: there are faster implementation of this
data
  Oneof
    fn
    fn'
    -- NOTE: everything needs to be quantified over the
    -- universe because otherwise it would be impossible
    -- to write higher-order functions (like `foldMap`).
    (fnU :: [Type] -> Type -> Type)
    (as :: [Type])
    b
  = OneofLeft !(fn fnU as b)
  | OneofRight !(fn' fnU as b)

-- | Build a balanced tree of `Oneof` from a list of function universes.
-- NOTE: it is important that this is a balanced tree as that reduces the amount of
-- overhead in `injectFn` and `extractFn` from `O(n)` to `O(log(n))`. Surprisingly,
-- we've observed this making more than 10% difference in runtime on some generation
-- tasks even though the list `as` is typically small (< 20 elements).
type family OneofL as where
  OneofL '[t] = t
  OneofL (t : ts) = Insert t (OneofL ts)

type family Insert a t where
  Insert a (Oneof fn fn') = Oneof fn' (Insert a fn)
  Insert a t = Oneof a t

-- | Compute the path to look up `fn` in `fnU`
type family MPath fn fnU :: Maybe [PathElem] where
  MPath fn fn = 'Just '[]
  MPath fn (Oneof fn' fn'' fnU) = Resolve (MPath fn (fn' fnU)) (MPath fn (fn'' fnU))
  MPath fn (Fix fn') = AddFix (MPath fn (fn' (Fix fn')))
  MPath _ _ = 'Nothing

type family AddFix mpath where
  AddFix ('Just path) = 'Just ('PFix ': path)
  AddFix 'Nothing = 'Nothing

type family Resolve a b where
  Resolve ('Just as) _ = 'Just ('PLeft ': as)
  Resolve _ ('Just bs) = 'Just ('PRight ': bs)
  Resolve _ _ = 'Nothing

type family FromJust m where
  FromJust ('Just a) = a

type family Path fn fn' where
  Path fn fn' = FromJust (MPath fn fn')

instance IsMember fn fn '[] where
  injectFn0 = id
  extractFn0 = Just

instance IsMember fn (fn' (Fix fn')) path => IsMember fn (Fix fn') ('PFix : path) where
  injectFn0 fn = Fix (injectFn0 @_ @_ @path fn)
  extractFn0 (Fix fn) = extractFn0 @_ @_ @path fn

instance IsMember fn (fn' fnU) path => IsMember fn (Oneof fn' fn'' fnU) ('PLeft : path) where
  injectFn0 fn = OneofLeft (injectFn0 @_ @_ @path fn)
  extractFn0 (OneofLeft fn) = extractFn0 @_ @_ @path fn
  extractFn0 _ = Nothing

instance IsMember fn (fn'' fnU) path => IsMember fn (Oneof fn' fn'' fnU) ('PRight : path) where
  injectFn0 fn = OneofRight (injectFn0 @_ @_ @path fn)
  extractFn0 (OneofRight fn) = extractFn0 @_ @_ @path fn
  extractFn0 _ = Nothing

instance (FunctionLike (fn fnU), FunctionLike (fn' fnU)) => FunctionLike (Oneof fn fn' fnU) where
  sem (OneofLeft f) = sem f
  sem (OneofRight f) = sem f

instance (Show (fn fnU as b), Show (fn' fnU as b)) => Show (Oneof fn fn' fnU as b) where
  show (OneofLeft fn) = show fn
  show (OneofRight fn') = show fn'

instance (Eq (fn fnU as b), Eq (fn' fnU as b)) => Eq (Oneof fn fn' fnU as b) where
  OneofLeft fn == OneofLeft fn' = fn == fn'
  OneofRight fn == OneofRight fn' = fn == fn'
  _ == _ = False

------------------------------------------------------------------------
-- Equality
------------------------------------------------------------------------

equalFn :: forall fn a. (Eq a, Member (EqFn fn) fn) => fn '[a, a] Bool
equalFn = injectFn (Equal @_ @fn)

data EqFn (fn :: [Type] -> Type -> Type) as b where
  Equal :: Eq a => EqFn fn '[a, a] Bool

deriving instance Eq (EqFn fn as b)
deriving instance Show (EqFn fn as b)

instance FunctionLike (EqFn fn) where
  sem Equal = (==)

------------------------------------------------------------------------
-- Booleans
------------------------------------------------------------------------

notFn :: forall fn. Member (BoolFn fn) fn => fn '[Bool] Bool
notFn = injectFn $ Not @fn

orFn :: forall fn. Member (BoolFn fn) fn => fn '[Bool, Bool] Bool
orFn = injectFn $ Or @fn

-- | Operations on Bool.
--   One might expect   And :: BoolFn fn '[Bool, Bool] Bool
--   But this is problematic because a Term like ( p x &&. q y ) has to solve for
--   x or y first, choose x, so once x is fixed, it might be impossible to solve for y
--   Luckily we can get conjuction by using Preds, in fact using Preds, the x and y
--   can even be the same variable.
--   Or can be problematic too (by using a form of DeMorgan's laws: x && y = not(not x || not y))
--   but while that is possible, there are many scenarios which can only be specified using Or.
--   If one inadvertantly uses a form of DeMorgan's laws, the worst that can happen is an ErrorSpec
--   is returned, and the user can reformulate using Preds. So Or is incomplete, but sound, when it works.
data BoolFn (fn :: [Type] -> Type -> Type) as b where
  Not :: BoolFn fn '[Bool] Bool
  Or :: BoolFn fn '[Bool, Bool] Bool

deriving instance Eq (BoolFn fn as b)
deriving instance Show (BoolFn fn as b)

instance FunctionLike (BoolFn fn) where
  sem Not = not
  sem Or = (||)

------------------------------------------------------------------------
-- Pairs
------------------------------------------------------------------------

data Prod a b = Prod {prodFst :: a, prodSnd :: b}
  deriving (Eq, Ord, Show)

fstFn :: forall fn a b. Member (PairFn fn) fn => fn '[Prod a b] a
fstFn = injectFn $ Fst @fn

sndFn :: forall fn a b. Member (PairFn fn) fn => fn '[Prod a b] b
sndFn = injectFn $ Snd @fn

pairFn :: forall fn a b. Member (PairFn fn) fn => fn '[a, b] (Prod a b)
pairFn = injectFn $ Pair @fn

data PairFn (fn :: [Type] -> Type -> Type) args res where
  Pair :: PairFn fn '[a, b] (Prod a b)
  Fst :: PairFn fn '[Prod a b] a
  Snd :: PairFn fn '[Prod a b] b

deriving instance Show (PairFn fn args res)
deriving instance Eq (PairFn fn args res)

instance FunctionLike (PairFn fn) where
  sem = \case
    Pair -> Prod
    Fst -> prodFst
    Snd -> prodSnd

type family ProdOver as where
  ProdOver '[] = ()
  ProdOver '[a] = a
  ProdOver (a : as) = Prod a (ProdOver as)

listToProd :: (ProdOver as -> r) -> List Identity as -> r
listToProd k Nil = k ()
listToProd k (Identity a :> Nil) = k a
listToProd k (Identity a :> b :> as) = k (Prod a (listToProd id (b :> as)))

prodToList :: forall as. TypeList as => ProdOver as -> List Identity as
prodToList = go (listShape @as)
  where
    go ::
      forall ts.
      List (Const ()) ts ->
      ProdOver ts ->
      List Identity ts
    go Nil _ = Nil
    go (_ :> Nil) a = Identity a :> Nil
    go (_ :> ix :> ixs) (Prod a as) = Identity a :> go (ix :> ixs) as

appendProd ::
  forall xs ys.
  (TypeList xs, TypeList ys) =>
  ProdOver xs ->
  ProdOver ys ->
  ProdOver (Append xs ys)
appendProd xs ys = listToProd id (appendList @Identity @xs @ys (prodToList xs) (prodToList ys))

splitProd ::
  forall xs ys.
  (TypeList xs, TypeList ys) =>
  ProdOver (Append xs ys) ->
  Prod (ProdOver xs) (ProdOver ys)
splitProd = go (listShape @xs) (listShape @ys)
  where
    go ::
      List (Const ()) as ->
      List (Const ()) bs ->
      ProdOver (Append as bs) ->
      Prod (ProdOver as) (ProdOver bs)
    go Nil _ p = Prod () p
    go (_ :> Nil) Nil p = Prod p ()
    go (_ :> Nil) (_ :> _) p = p
    go (_ :> a :> as) bs (Prod x xs) = Prod (Prod x p0) p1
      where
        Prod p0 p1 = go (a :> as) bs xs

------------------------------------------------------------------------
-- Sums
------------------------------------------------------------------------

data Sum a b
  = SumLeft a
  | SumRight b
  deriving (Ord, Eq, Show)

injLeftFn :: forall fn a b. Member (SumFn fn) fn => fn '[a] (Sum a b)
injLeftFn = injectFn @(SumFn fn) @fn InjLeft

injRightFn :: forall fn a b. Member (SumFn fn) fn => fn '[b] (Sum a b)
injRightFn = injectFn @(SumFn fn) @fn InjRight

data SumFn (fn :: [Type] -> Type -> Type) args res where
  InjLeft :: SumFn fn '[a] (Sum a b)
  InjRight :: SumFn fn '[b] (Sum a b)

deriving instance Show (SumFn fn args res)
deriving instance Eq (SumFn fn args res)

instance FunctionLike (SumFn fn) where
  sem = \case
    InjLeft -> SumLeft
    InjRight -> SumRight

type family SumOver as where
  SumOver '[a] = a
  SumOver (a : as) = Sum a (SumOver as)

------------------------------------------------------------------------
-- Sets
------------------------------------------------------------------------

singletonFn :: forall fn a. (Member (SetFn fn) fn, Ord a) => fn '[a] (Set a)
singletonFn = injectFn $ Singleton @_ @fn

unionFn :: forall fn a. (Member (SetFn fn) fn, Ord a) => fn '[Set a, Set a] (Set a)
unionFn = injectFn $ Union @_ @fn

subsetFn :: forall fn a. (Member (SetFn fn) fn, Ord a) => fn '[Set a, Set a] Bool
subsetFn = injectFn $ Subset @_ @fn

memberFn :: forall fn a. (Member (SetFn fn) fn, Ord a) => fn '[a, Set a] Bool
memberFn = injectFn $ Member @_ @fn

elemFn :: forall fn a. (Member (SetFn fn) fn, Eq a) => fn '[a, [a]] Bool
elemFn = injectFn $ Elem @_ @fn

disjointFn :: forall fn a. (Member (SetFn fn) fn, Ord a) => fn '[Set a, Set a] Bool
disjointFn = injectFn $ Disjoint @_ @fn

fromListFn :: forall fn a. (Member (SetFn fn) fn, Ord a) => fn '[[a]] (Set a)
fromListFn = injectFn $ FromList @_ @fn

data SetFn (fn :: [Type] -> Type -> Type) args res where
  Subset :: Ord a => SetFn fn '[Set a, Set a] Bool
  Disjoint :: Ord a => SetFn fn '[Set a, Set a] Bool
  Member :: Ord a => SetFn fn '[a, Set a] Bool
  Singleton :: Ord a => SetFn fn '[a] (Set a)
  Union :: Ord a => SetFn fn '[Set a, Set a] (Set a)
  Elem :: Eq a => SetFn fn '[a, [a]] Bool
  FromList :: Ord a => SetFn fn '[[a]] (Set a)

deriving instance Show (SetFn fn args res)
deriving instance Eq (SetFn fn args res)

instance FunctionLike (SetFn fn) where
  sem = \case
    Subset -> Set.isSubsetOf
    Disjoint -> Set.disjoint
    Member -> Set.member
    Singleton -> Set.singleton
    Union -> (<>)
    Elem -> elem
    FromList -> Set.fromList

------------------------------------------------------------------------
-- Maps
------------------------------------------------------------------------

data MapFn (fn :: [Type] -> Type -> Type) args res where
  Dom :: Ord k => MapFn fn '[Map k v] (Set k)
  Rng :: MapFn fn '[Map k v] [v]

deriving instance Show (MapFn fn args res)
deriving instance Eq (MapFn fn args res)

instance FunctionLike (MapFn fn) where
  sem = \case
    Dom -> Map.keysSet
    Rng -> Map.elems

domFn :: forall fn k v. (Member (MapFn fn) fn, Ord k) => fn '[Map k v] (Set k)
domFn = injectFn $ Dom @_ @fn

rngFn :: forall fn k v. Member (MapFn fn) fn => fn '[Map k v] [v]
rngFn = injectFn $ Rng @fn
