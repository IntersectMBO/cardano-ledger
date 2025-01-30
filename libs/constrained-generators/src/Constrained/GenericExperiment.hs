{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Constrained.GenericExperiment where

import GHC.TypeLits(Symbol)
-- import Data.Orphans -- instances on Symbol
import Data.Kind
import Constrained.List
import GHC.Generics
import Data.Functor.Identity
import Data.Functor.Const

-- Sum and Prod and their operations came from Constrained.Univ
-- Since that file is basically about Fn, which is going to disappear
-- this seems like the appropriate place for them. We will need FunctionSymbol's
-- for fst_ snd_ pair_ injectLeft_  and injectRight_ defined someplace else
------------------------------------------------------------------------
-- Pairs
------------------------------------------------------------------------

data Prod a b = Prod {prodFst :: a, prodSnd :: b}
  deriving (Eq, Ord, Show)

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

{-
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
-}
------------------------------------------------------------------------
-- Sums
------------------------------------------------------------------------

-- | Generic way to represent Sums
data Sum a b
  = SumLeft a
  | SumRight b
  deriving (Ord, Eq, Show)

type family SumOver as where
  SumOver '[a] = a
  SumOver (a : as) = Sum a (SumOver as)

{-  This should be a Sum Witness type?
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
-}

-----------------------------------------------------------------
-- Generics
-----------------------------------------------------------------

{- This part of the code base is responsible for implementing the conversion
   from a `Generic` type to a `Sum` over `Prod` representation that automatically
   gives you an instance of `HasSpec`. The user has three options for building their
   own instances of `HasSpec`, either they hand-roll an instance, they go with the
   entirely `Generic` instance, or they provide their own `SimpleRep` for their type.

   The latter may be appropriate when the type is an optimized representation:

   ```
   newtype Foo = Foo { unFoo :: MemoBytes ActualFoo }

   instance HasSimpleRep Foo where
     type SimpleRep Foo = ActualFoo
     toSimpleRep = unMemoBytes . unFoo
     fromSimpleRep = Foo . memoBytes
   ```

   This would then allow for `Foo` to be treated as a simple `newtype` over `ActualFoo`
   in constraints:

   ```
   fooSpec :: Specification Foo
   fooSpec = constrained $ \ foo ->
     match foo $ \ actualFoo -> ...
   ```
-}

-- Building a SOP type (Sum Of Prod) --------------------------------------

-- | A constructor name with the types of its arguments
data (c :: Symbol) ::: (ts :: [Type])

-- | Turn a `Rep` into a list that flattens the sum
-- structre and gives the constructors names:
--   Maybe Int -> '["Nothing" ::: '[()], "Just" ::: '[Int]]
--   Either Int Bool -> '["Left" ::: '[Int], "Right" ::: '[Bool]]
--   data Foo = Foo Int Bool | Bar Double -> '["Foo" ::: '[Int, Bool], "Bar" ::: '[Double]]
type family SOPOf f where
  SOPOf (D1 _ f) = SOPOf f
  SOPOf (f :+: g) = Append (SOPOf f) (SOPOf g)
  SOPOf (C1 ('MetaCons constr _ _) f) = '[constr ::: Constr f]

-- | Flatten a single constructor
type family Constr f where
  -- TODO: Here we should put in the selector names
  -- so that they can be re-used to create selectors more
  -- easily than the current disgusting `Fst . Snd . Snd . Snd ...`
  -- method.
  Constr (S1 _ f) = Constr f
  Constr (K1 _ k) = '[k]
  Constr U1 = '[()]
  Constr (f :*: g) = Append (Constr f) (Constr g)

-- | Turn a list from `SOPOf` into a `Sum` over
-- `Prod` representation.
type family SOP constrs where
  SOP '[c ::: prod] = ProdOver prod
  SOP ((c ::: prod) : constrs) = Sum (ProdOver prod) (SOP constrs)

-- Constructing a SOP -----------------------------------------------------

type family ConstrOf c sop where
  ConstrOf c (c ::: constr : sop) = constr
  ConstrOf c (_ : sop) = ConstrOf c sop

class Inject c constrs r where
  inject' :: (SOP constrs -> r) -> FunTy (ConstrOf c constrs) r

instance TypeList prod => Inject c '[c ::: prod] r where
  inject' k = curryList_ @prod Identity (listToProd k)

instance TypeList prod => Inject c ((c ::: prod) : prod' : constrs) r where
  inject' k = curryList_ @prod Identity (listToProd (k . SumLeft @_ @(SOP (prod' : constrs))))

instance
  {-# OVERLAPPABLE #-}
  ( FunTy (ConstrOf c ((c' ::: prod) : con : constrs)) r ~ FunTy (ConstrOf c (con : constrs)) r
  , -- \^ An unfortunately roundabout way of saying `c !~ c'`
    Inject c (con : constrs) r
  ) =>
  Inject c ((c' ::: prod) : con : constrs) r
  where
  inject' k = inject' @c @(con : constrs) (k . SumRight)

inject ::
  forall c constrs. Inject c constrs (SOP constrs) => FunTy (ConstrOf c constrs) (SOP constrs)
inject = inject' @c @constrs id

-- Deconstructing a SOP ---------------------------------------------------

-- | An `ALG constrs r` is a function that takes a way to turn every
-- constructor into an `r`:
-- ```
-- ALG (SOPOf (Rep (Either Int Bool))) r = (Int -> r) -> (Bool -> r) -> r
-- ```
type family ALG constrs r where
  ALG '[c ::: prod] r = FunTy prod r -> r
  ALG ((c ::: prod) : constrs) r = FunTy prod r -> ALG constrs r

class SOPLike constrs r where
  -- | Run a `SOP`
  algebra :: SOP constrs -> ALG constrs r

  -- | Ignore everything in the `SOP`
  consts :: r -> ALG constrs r

instance TypeList prod => SOPLike '[c ::: prod] r where
  algebra prod f = uncurryList_ @prod runIdentity f $ prodToList prod
  consts r _ = r

instance (TypeList prod, SOPLike (con : cases) r) => SOPLike ((c ::: prod) : con : cases) r where
  algebra (SumLeft prod) f = consts @(con : cases) @r (algebra @'[c ::: prod] prod f)
  algebra (SumRight rest) _ = algebra @(con : cases) @r rest

  consts r _ = consts @(con : cases) r


-- The individual constructor level ---------------------------------------

class SimpleConstructor rep where
  toSimpleCon' :: rep p -> ProdOver (Constr rep)
  fromSimpleCon' :: ProdOver (Constr rep) -> rep p
instance
  ( SimpleConstructor f
  , SimpleConstructor g
  , TypeList (Constr f)
  , TypeList (Constr g)
  ) =>
  SimpleConstructor (f :*: g)
  where
  toSimpleCon' (a :*: b) = appendProd @(Constr f) @(Constr g) (toSimpleCon' a) (toSimpleCon' b)
  fromSimpleCon' constr =
    let Prod a b = splitProd @(Constr f) @(Constr g) constr
     in (fromSimpleCon' a :*: fromSimpleCon' b)

instance SimpleConstructor f => SimpleConstructor (S1 s f) where
  toSimpleCon' (M1 f) = toSimpleCon' f
  fromSimpleCon' a = M1 (fromSimpleCon' a)

instance SimpleConstructor (K1 i k) where
  toSimpleCon' (K1 k) = k
  fromSimpleCon' k = K1 k

instance SimpleConstructor U1 where
  toSimpleCon' U1 = ()
  fromSimpleCon' _ = U1

-- The sum type level -----------------------------------------------------

-- | Construct and deconstruct cases in a `SOP`
class SopList xs ys where
  injectSOPLeft :: SOP xs -> SOP (Append xs ys)
  injectSOPRight :: SOP ys -> SOP (Append xs ys)
  caseSOP :: SOP (Append xs ys) -> Sum (SOP xs) (SOP ys)

instance SopList '[c ::: x] (y : ys) where
  injectSOPLeft = SumLeft
  injectSOPRight = SumRight
  caseSOP = id

instance SopList (x' : xs) (y : ys) => SopList (c ::: x : x' : xs) (y : ys) where
  injectSOPLeft (SumLeft a) = SumLeft a
  injectSOPLeft (SumRight b) = SumRight (injectSOPLeft @(x' : xs) @(y : ys) b)

  injectSOPRight a = SumRight (injectSOPRight @(x' : xs) @(y : ys) a)

  caseSOP (SumLeft a) = SumLeft (SumLeft a)
  caseSOP (SumRight b) = case caseSOP @(x' : xs) @(y : ys) b of
    SumLeft b' -> SumLeft (SumRight b')
    SumRight b' -> SumRight b'
