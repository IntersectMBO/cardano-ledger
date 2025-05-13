{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | How can we automatically inject normal Haskell types into the logic, using GHC.Generics
module Constrained.Generic where

import Constrained.List (
  Append,
  FunTy,
  List (..),
  TypeList,
  appendList,
  curryList_,
  listShape,
  uncurryList_,
 )
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Data.Typeable
import GHC.Generics
import GHC.TypeLits (Symbol)

-- Sum and Prod and their operations came from Constrained.Univ
-- Since that file is basically about Fn, which is going to disappear
-- this seems like the appropriate place for them. We will need FunctionSymbol's
-- for fst_ snd_ pair_ injectLeft_  and injectRight_ defined someplace else
------------------------------------------------------------------------
-- Pairs
------------------------------------------------------------------------

data Prod a b = Prod {prodFst :: a, prodSnd :: b}
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (Prod a b) where
  show (Prod x y) = "(Prod " ++ show x ++ " " ++ show y ++ ")"

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

-- | Generic way to represent Sums
data Sum a b
  = SumLeft a
  | SumRight b
  deriving (Ord, Eq, Show)

type family SumOver as where
  SumOver '[a] = a
  SumOver (a : as) = Sum a (SumOver as)

-- =========================================================================
-- The idea is for each type, we define a type family (HasSimpleRep) the maps
-- that type to another type we already know how to deal with. The methods
-- 'toSimpleRep' and 'fromSimpleRep' cature that knowledge. The strategy
-- we want to use most of the time, is to use GHC.Generics, to construct
-- the SimpleRep out of Sum and Prod, and to write the 'toSimpleRep' and
-- 'fromSimpleRep' methods automatically. If we can do that, then
-- every thing else will come for free. Note that it is not REQUIRED to make
-- the (SimpleRep t) out of Sum and Prod, but it helps. Note the default
-- method instances use Sum and Prod, but that is not required.
-- ==========================================================================

class Typeable (SimpleRep a) => HasSimpleRep a where
  type SimpleRep a
  type TheSop a :: [Type]
  toSimpleRep :: a -> SimpleRep a
  fromSimpleRep :: SimpleRep a -> a

  type TheSop a = SOPOf (Rep a)
  type SimpleRep a = SOP (TheSop a)

  default toSimpleRep ::
    ( Generic a
    , SimpleGeneric (Rep a)
    , SimpleRep a ~ SimplifyRep (Rep a)
    ) =>
    a ->
    SimpleRep a
  toSimpleRep = toSimpleRep' . from

  default fromSimpleRep ::
    ( Generic a
    , SimpleGeneric (Rep a)
    , SimpleRep a ~ SimplifyRep (Rep a)
    ) =>
    SimpleRep a ->
    a
  fromSimpleRep = to . fromSimpleRep'

type family SimplifyRep f where
  SimplifyRep f = SOP (SOPOf f)

instance HasSimpleRep Bool

instance HasSimpleRep () where
  type SimpleRep () = ()
  toSimpleRep x = x
  fromSimpleRep x = x

-- ===============================================================
-- How to move back and forth from (SimpleRep a) to 'a' in a
-- generic way, derived by the Generics machinery is captured
-- by the SimpleGeneric class
-- ===============================================================

class SimpleGeneric rep where
  toSimpleRep' :: rep p -> SimplifyRep rep
  fromSimpleRep' :: SimplifyRep rep -> rep p

instance SimpleGeneric f => SimpleGeneric (D1 d f) where
  toSimpleRep' (M1 f) = toSimpleRep' f
  fromSimpleRep' a = M1 (fromSimpleRep' a)

instance
  ( SimpleGeneric f
  , SimpleGeneric g
  , SopList (SOPOf f) (SOPOf g)
  ) =>
  SimpleGeneric (f :+: g)
  where
  toSimpleRep' (L1 f) = injectSOPLeft @(SOPOf f) @(SOPOf g) $ toSimpleRep' f
  toSimpleRep' (R1 g) = injectSOPRight @(SOPOf f) @(SOPOf g) $ toSimpleRep' g
  fromSimpleRep' sop =
    case caseSOP @(SOPOf f) @(SOPOf g) sop of
      SumLeft l -> L1 (fromSimpleRep' l)
      SumRight r -> R1 (fromSimpleRep' r)

instance SimpleConstructor f => SimpleGeneric (C1 ('MetaCons c a b) f) where
  toSimpleRep' (M1 f) = toSimpleCon' f
  fromSimpleRep' a = M1 (fromSimpleCon' a)

-- ================================================================================
--    This part of the code base is responsible for implementing the conversion
--    from a `Generic` type to a `Sum` over `Prod` representation that automatically
--    gives you an instance of `HasSpec`. The user has three options for building their
--    own instances of `HasSpec`, either they hand-roll an instance, they go with the
--    entirely `Generic` instance, or they provide their own `SimpleRep` for their type.
--
--    The latter may be appropriate when the type is an optimized representation:
--
--    ```
--    newtype Foo = Foo { unFoo :: MemoBytes ActualFoo }
--
--    instance HasSimpleRep Foo where
--      type SimpleRep Foo = ActualFoo
--      toSimpleRep = unMemoBytes . unFoo
--      fromSimpleRep = Foo . memoBytes
--    ```
--
--    This would then allow for `Foo` to be treated as a simple `newtype` over `ActualFoo`
--    in constraints:
--
--    ```
--    fooSpec :: Specification Foo
--    fooSpec = constrained $ \ foo ->
--      match foo $ \ actualFoo -> ...
--    ```
-- =========================================================================================

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

-- =====================================================
-- Constructing a SOP ----------------------------------

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

-- =====================================================
-- Deconstructing a SOP --------------------------------

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

-- ========================================================
-- The individual constructor level -----------------------

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

-- ===================================================
-- The sum type level --------------------------------

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

-- ===========================================================
-- How it works
-- If the TypeSpec method of the HasSpec class has a SimpleRep instance, Like this
-- type TypeSpec = a
-- where 'a' has a Sum Product representation then all of the other methods
-- can use the default implementation. This saves lots of trouble for mundane types.
--
-- `HasSimpleRep` and `GenericsFn` are meant to allow you to express that a
-- type is isomorphic to some other type 't' that has a (HasSpec t) instance.
--
-- The trick is that the default instance of `HasSpec a` assumes
-- `HasSimpleRep a` and defines `TypeSpec a = TypeSpec (SimpleRep a)`.
--
-- From this it's possible to work with things of type `a` in constraints by
-- treating them like things of type `SimpleRep a`. This allows us to do case
-- matching etc. on `a` when `SimpleRep a` is a `Sum` type, for example.
--
-- Or alternatively it allows us to treat `a` as a newtype over `SimpleRep a`
-- when using `match`.
-- ====================================================================
