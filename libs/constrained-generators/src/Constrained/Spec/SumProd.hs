{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | A lot of the surface-syntax related to generics
module Constrained.Spec.SumProd (
  IsNormalType,
  ProdAsListComputes,
  IsProductType,
  caseOn,
  branch,
  branchW,
  forAll',
  constrained',
  reify',
  con,
  onCon,
  isCon,
  sel,
  match,
  onJust,
  isJust,
  chooseSpec,
  left_,
  right_,
  just_,
  nothing_,
  fst_,
  snd_,
  pair_,
  prodFst_,
  prodSnd_,
  prod_,
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.Generation
import Constrained.Generic
import Constrained.List
import Constrained.Spec.List
import Constrained.Syntax
import Constrained.TheKnot
import Constrained.TypeErrors
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.TypeLits (Symbol)
import GHC.TypeNats
import Test.QuickCheck (Arbitrary (..), oneof)

------------------------------------------------------------------------
-- Syntax for `(,)` and `Either`
------------------------------------------------------------------------

-- | `fst` in `Term` form
fst_ :: (HasSpec x, HasSpec y) => Term (x, y) -> Term x
fst_ = prodFst_ . toGeneric_

-- | `snd` in `Term` form
snd_ :: (HasSpec x, HasSpec y) => Term (x, y) -> Term y
snd_ = prodSnd_ . toGeneric_

-- | `(,)` in `Term` form
pair_ ::
  ( HasSpec a
  , HasSpec b
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term a ->
  Term b ->
  Term (a, b)
pair_ x y = fromGeneric_ $ prod_ x y

-- | `Left` in `Term` form
left_ ::
  ( HasSpec a
  , HasSpec b
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term a ->
  Term (Either a b)
left_ = fromGeneric_ . injLeft_

-- | `Right` in `Term` form
right_ ::
  ( HasSpec a
  , HasSpec b
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term b ->
  Term (Either a b)
right_ = fromGeneric_ . injRight_

-- | @case .. of@ for `Term` and `Pred`. Note that the arguments
-- here are @`Weighted` `Binder`@ over all the `Cases` of the
-- `SimpleRep` of the scrutinee. The `Binder`s can be constructed with
-- `branch` and `branchW`.
caseOn ::
  forall a.
  ( GenericRequires a
  , SimpleRep a ~ SumOver (Cases (SimpleRep a))
  , TypeList (Cases (SimpleRep a))
  ) =>
  Term a ->
  FunTy (MapList (Weighted Binder) (Cases (SimpleRep a))) Pred
caseOn tm = curryList @(Cases (SimpleRep a)) (mkCase (toGeneric_ tm))

-- | Build a branch in a `caseOn`
branch ::
  forall p a.
  ( HasSpec a
  , All HasSpec (Args a)
  , IsPred p
  , IsProd a
  ) =>
  FunTy (MapList Term (Args a)) p ->
  Weighted Binder a
branch body =
  -- NOTE: It's not sufficient to simply apply `body` to all the arguments
  -- with `uncurryList` because that will mean that `var` is repeated in the
  -- body. For example, consider `branch $ \ i j -> i <=. j`. If we don't
  -- build the lets this will boil down to `p :-> fst p <=. snd p` which
  -- will blow up at generation time. If we instead do: `p :-> Let x (fst p) (Let y (snd p) (x <=. y))`
  -- the solver will solve `x` and `y` separately (`y` before `x` in this case) and things
  -- will work just fine.
  Weighted Nothing (bind (buildBranch @p body . toArgs @a))

-- | Build a branch in a `caseOn` with a weight attached.
branchW ::
  forall p a.
  ( HasSpec a
  , All HasSpec (Args a)
  , IsPred p
  , IsProd a
  ) =>
  Int ->
  FunTy (MapList Term (Args a)) p ->
  Weighted Binder a
branchW w body =
  Weighted (Just w) (bind (buildBranch @p body . toArgs @a))

-- ====================================================
-- All the magic for things like 'caseOn', 'match', forAll' etc. lives here.
-- Classes and type families about Sum, Prod, construtors, selectors
-- These let us express the types of things like 'match' and 'caseOn'

class IsProd p where
  toArgs ::
    HasSpec p => Term p -> List Term (Args p)

instance {-# OVERLAPPABLE #-} Args a ~ '[a] => IsProd a where
  toArgs = (:> Nil)

instance IsProd b => IsProd (Prod a b) where
  toArgs (p :: Term (Prod a b))
    | Evidence <- prerequisites @(Prod a b) = prodFst_ p :> toArgs (prodSnd_ p)

type family Args t where
  Args (Prod a b) = a : Args b
  Args a = '[a]

type family ResultType t where
  ResultType (a -> b) = ResultType b
  ResultType a = a

-- | A normal type, not an underlying generic representation using `Sum` and t`Prod`
type IsNormalType a =
  ( AssertComputes
      (Cases a)
      ( Text "Failed to compute Cases in a use of IsNormalType for "
          :$$: ShowType a
          :<>: Text ", are you missing an IsNormalType constraint?"
      )
  , Cases a ~ '[a]
  , AssertComputes
      (Args a)
      ( Text "Failed to compute Args in a use of IsNormalType for "
          :<>: ShowType a
          :<>: Text ", are you missing an IsNormalType constraint?"
      )
  , Args a ~ '[a]
  , IsProd a
  , CountCases a ~ 1
  )

type family Cases t where
  Cases (Sum a b) = a : Cases b
  Cases a = '[a]

-- | A single-constructor type like t`(,)`
type IsProductType a =
  ( HasSimpleRep a
  , AssertComputes
      (Cases (SimpleRep a))
      ( Text "Failed to compute Cases in a use of IsProductType for "
          :$$: ShowType a
          :<>: Text ", are you missing an IsProductType constraint?"
      )
  , Cases (SimpleRep a) ~ '[SimpleRep a]
  , SimpleRep a ~ SumOver (Cases (SimpleRep a))
  , IsProd (SimpleRep a)
  , HasSpec (SimpleRep a)
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , All HasSpec (Args (SimpleRep a))
  )

type ProductAsList a = Args (SimpleRep a)

class HasSpec (SOP sop) => SOPTerm c sop where
  inj_ :: Term (ProdOver (ConstrOf c sop)) -> Term (SOP sop)

instance HasSpec (ProdOver constr) => SOPTerm c (c ::: constr : '[]) where
  inj_ = id

instance
  ( HasSpec (SOP (con : sop))
  , HasSpec (ProdOver constr)
  , KnownNat (CountCases (SOP (con : sop)))
  ) =>
  SOPTerm c (c ::: constr : con : sop)
  where
  inj_ = injLeft_

instance
  {-# OVERLAPPABLE #-}
  ( HasSpec (ProdOver con)
  , SOPTerm c (con' : sop)
  , ConstrOf c (con' : sop) ~ ConstrOf c ((c' ::: con) : con' : sop)
  , KnownNat (CountCases (SOP (con' : sop)))
  ) =>
  SOPTerm c ((c' ::: con) : con' : sop)
  where
  inj_ = injRight_ . inj_ @c @(con' : sop)

class HasSpec (ProdOver constr) => ConstrTerm constr where
  prodOver_ :: List Term constr -> Term (ProdOver constr)

instance HasSpec a => ConstrTerm '[a] where
  prodOver_ (a :> Nil) = a

type family At n as where
  At 0 (a : as) = a
  At n (a : as) = At (n - 1) as

class Select n as where
  select_ :: Term (ProdOver as) -> Term (At n as)

instance Select 0 (a : '[]) where
  select_ = id

instance (HasSpec a, HasSpec (ProdOver (a' : as))) => Select 0 (a : a' : as) where
  select_ = prodFst_

instance
  {-# OVERLAPPABLE #-}
  ( HasSpec a
  , HasSpec (ProdOver (a' : as))
  , At (n - 1) (a' : as) ~ At n (a : a' : as)
  , Select (n - 1) (a' : as)
  ) =>
  Select n (a : a' : as)
  where
  select_ = select_ @(n - 1) @(a' : as) . prodSnd_

class IsConstrOf (c :: Symbol) b sop where
  mkCases ::
    (HasSpec b, All HasSpec (Cases (SOP sop))) =>
    (forall a. Term a -> Pred) ->
    (Term b -> Pred) ->
    List (Weighted Binder) (Cases (SOP sop))

instance
  ( b ~ ProdOver as
  , TypeList (Cases (SOP (con : sop)))
  ) =>
  IsConstrOf c b ((c ::: as) : con : sop)
  where
  mkCases r (k :: Term b -> Pred) =
    Weighted Nothing (bind k)
      :> mapListC @HasSpec (\_ -> Weighted Nothing (bind r)) (listShape @(Cases (SOP (con : sop))))

instance
  ( b ~ ProdOver as
  , IsNormalType b
  ) =>
  IsConstrOf c b '[c ::: as]
  where
  mkCases _ (k :: Term b -> Pred) = Weighted Nothing (bind k) :> Nil

instance
  {-# OVERLAPPABLE #-}
  ( Cases (SOP ((c' ::: as) : cs)) ~ (ProdOver as : Cases (SOP cs))
  , IsConstrOf c b cs
  ) =>
  IsConstrOf c b ((c' ::: as) : cs)
  where
  mkCases r k = Weighted Nothing (bind (r @(ProdOver as))) :> mkCases @c @_ @cs r k

-- Instances --------------------------------------------------------------

fstW :: (HasSpec a, HasSpec b) => FunW '[(a, b)] a
fstW = ComposeW ProdFstW ToGenericW

sndW :: (HasSpec a, HasSpec b) => FunW '[(a, b)] b
sndW = ComposeW ProdSndW ToGenericW

instance
  (HasSpec a, HasSpec b, Arbitrary (FoldSpec a), Arbitrary (FoldSpec b)) =>
  Arbitrary (FoldSpec (a, b))
  where
  arbitrary =
    oneof
      [ preMapFoldSpec (Fun fstW) <$> arbitrary
      , preMapFoldSpec (Fun sndW) <$> arbitrary
      , pure NoFold
      ]
  shrink NoFold = []
  shrink FoldSpec {} = [NoFold]

buildBranch ::
  forall p as.
  ( All HasSpec as
  , IsPred p
  ) =>
  FunTy (MapList Term as) p ->
  List Term as ->
  Pred
buildBranch bd Nil = toPred bd
buildBranch bd (t :> args) =
  letBind t $ \x -> buildBranch @p (bd x) args

-- | ProdAsListComputes is here to make sure that in situations like this:
--
-- > type family Foobar k
-- >
-- > ex :: HasSpec (Foobar k) => Specification (Int, Foobar k)
-- > ex = constrained $ \ p -> match p $ \ i _ -> (i ==. 10)
--
-- Where you're trying to work with an unevaluated type family in constraints.
-- You get reasonable type errors prompting you to add the @IsNormalType (Foobar k)@ constraint
-- like this:
--
-- >     • Type list computation is stuck on
-- >         Args (Foobar k)
-- >       Have you considered adding an IsNormalType or ProdAsListComputes constraint?
-- >     • In the first argument of ‘($)’, namely ‘match p’
-- >       In the expression: match p $ \ i _ -> (i ==. 10)
-- >       In the second argument of ‘($)’, namely
-- >         ‘\ p -> match p $ \ i _ -> (i ==. 10)’
-- >     |
-- > 503 | ex = constrained $ \ p -> match p $ \ i _ -> (i ==. 10)
-- >     |                           ^^^^^
--
-- Which should help you come to the conclusion that you need to do something
-- like this for everything to compile:
--
-- > ex :: (HasSpec (Foobar k), IsNormalType (Foobar k)) => Specification (Int, Foobar k)
type ProdAsListComputes a =
  AssertSpineComputes
    (Text "Have you considered adding an IsNormalType or ProdAsListComputes constraint?")
    (ProductAsList a)

-- | Pattern-match on a product type and build constraints with the constituents:
match ::
  forall p a.
  ( IsProductType a
  , IsPred p
  , GenericRequires a
  , ProdAsListComputes a
  ) =>
  Term a -> FunTy (MapList Term (ProductAsList a)) p -> Pred
match p m = caseOn p (branch @p m)

-- NOTE: `ResultType r ~ Term a` is NOT a redundant constraint,
-- removing it causes type inference to break elsewhere

-- | Create a constructor @c@:
-- > just_ :: (HasSpec a, IsNormalType a) => Term a -> Term (Maybe a)
-- > just_ = con @"Just"
con ::
  forall c a r.
  ( SimpleRep a ~ SOP (TheSop a)
  , TypeSpec a ~ TypeSpec (SOP (TheSop a))
  , TypeList (ConstrOf c (TheSop a))
  , r ~ FunTy (MapList Term (ConstrOf c (TheSop a))) (Term a)
  , ResultType r ~ Term a
  , SOPTerm c (TheSop a)
  , ConstrTerm (ConstrOf c (TheSop a))
  , GenericRequires a
  ) =>
  r
con =
  curryList @(ConstrOf c (TheSop a)) @Term
    (fromGeneric_ @a . inj_ @c @(TheSop a) . prodOver_)

-- | `Term`-level `Just`
just_ :: (HasSpec a, IsNormalType a) => Term a -> Term (Maybe a)
just_ = con @"Just"

-- | `Term`-level `Nothing`
nothing_ :: (HasSpec a, IsNormalType a) => Term (Maybe a)
nothing_ = con @"Nothing" (Lit ())

-- | Select a specific field from a single-constructor type:
-- > data Record = Record { foo :: Int, bar :: Bool }
-- > foo_ :: Term Record -> Term Int
-- > foo_ = sel @0
-- > bar_ :: Term Record -> Term Bool
-- > bar_ = sel @1
sel ::
  forall n a c as.
  ( SimpleRep a ~ ProdOver as
  , -- TODO: possibly investigate deriving this from the actual SOP of SimpleRep, as currently it's buggy if you define
    -- your own custom SOP-like SimpleRep by defining SimpleRep rather than TheSop (it's stupid I know)
    TheSop a ~ '[c ::: as]
  , TypeSpec a ~ TypeSpec (ProdOver as)
  , Select n as
  , HasSpec a
  , HasSpec (ProdOver as)
  , HasSimpleRep a
  , GenericRequires a
  ) =>
  Term a ->
  Term (At n as)
sel = select_ @n @as . toGeneric_

-- | Like `forAll` but pattern matches on the `Term a`
forAll' ::
  forall t a p.
  ( Forallable t a
  , Cases (SimpleRep a) ~ '[SimpleRep a]
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , HasSpec t
  , HasSpec (SimpleRep a)
  , HasSimpleRep a
  , All HasSpec (Args (SimpleRep a))
  , IsPred p
  , IsProd (SimpleRep a)
  , IsProductType a
  , HasSpec a
  , GenericRequires a
  , ProdAsListComputes a
  ) =>
  Term t ->
  FunTy (MapList Term (ProductAsList a)) p ->
  Pred
forAll' xs f = forAll xs $ \x -> match @p x f

-- | Like `constrained` but pattern matches on the bound `Term a`
constrained' ::
  forall a p.
  ( Cases (SimpleRep a) ~ '[SimpleRep a]
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , HasSpec (SimpleRep a)
  , HasSimpleRep a
  , All HasSpec (Args (SimpleRep a))
  , IsProd (SimpleRep a)
  , HasSpec a
  , IsProductType a
  , IsPred p
  , GenericRequires a
  , ProdAsListComputes a
  ) =>
  FunTy (MapList Term (ProductAsList a)) p ->
  Specification a
constrained' f = constrained $ \x -> match @p x f

-- | Like `reify` but pattern matches on the bound `Term b`
reify' ::
  forall a b p.
  ( Cases (SimpleRep b) ~ '[SimpleRep b]
  , TypeSpec b ~ TypeSpec (SimpleRep b)
  , HasSpec (SimpleRep b)
  , HasSimpleRep b
  , All HasSpec (Args (SimpleRep b))
  , IsProd (SimpleRep b)
  , HasSpec a
  , HasSpec b
  , IsProductType b
  , IsProd a
  , IsPred p
  , GenericRequires b
  , ProdAsListComputes b
  ) =>
  Term a ->
  (a -> b) ->
  FunTy (MapList Term (ProductAsList b)) p ->
  Pred
reify' a r f = reify a r $ \x -> match @p x f

instance
  ( HasSpec a
  , HasSpec (ProdOver (a : b : as))
  , ConstrTerm (b : as)
  ) =>
  ConstrTerm (a : b : as)
  where
  prodOver_ (a :> as) = prod_ a (prodOver_ as)

-- TODO: the constraints around this are horrible!! We should figure out a way to make these things nicer.

-- | `caseOn` a _single_ constructor only
onCon ::
  forall c a p.
  ( IsConstrOf c (ProdOver (ConstrOf c (TheSop a))) (TheSop a)
  , GenericRequires a
  , SumOver (Cases (SOP (TheSop a))) ~ SimpleRep a
  , All HasSpec (Cases (SOP (TheSop a)))
  , HasSpec (ProdOver (ConstrOf c (TheSop a)))
  , IsPred p
  , Args (ProdOver (ConstrOf c (TheSop a))) ~ ConstrOf c (TheSop a)
  , All HasSpec (ConstrOf c (TheSop a))
  , IsProd (ProdOver (ConstrOf c (TheSop a)))
  ) =>
  Term a ->
  FunTy (MapList Term (ConstrOf c (TheSop a))) p ->
  Pred
onCon tm p =
  Case
    (toGeneric_ tm)
    ( mkCases @c @(ProdOver (ConstrOf c (TheSop a))) @(TheSop a)
        (const $ Assert (Lit True))
        (buildBranch @p p . toArgs)
    )

-- | Check if a value is an instance of a specific constructor:
-- > isJustConstraint :: HasSpec a => Term (Maybe a) -> Pred
-- > isJustConstraint t = isCon @"Just" t
isCon ::
  forall c a.
  ( IsConstrOf c (ProdOver (ConstrOf c (TheSop a))) (TheSop a)
  , SumOver (Cases (SOP (TheSop a))) ~ SimpleRep a
  , All HasSpec (Cases (SOP (TheSop a)))
  , HasSpec (ProdOver (ConstrOf c (TheSop a)))
  , GenericRequires a
  ) =>
  Term a ->
  Pred
isCon tm =
  Case
    (toGeneric_ tm)
    ( mkCases @c @(ProdOver (ConstrOf c (TheSop a))) @(TheSop a)
        (const $ Assert (Lit False))
        (const $ Assert (Lit True))
    )

-- | `onCon` specialized to `Just`
onJust ::
  forall a p.
  (HasSpec a, IsNormalType a, IsPred p) =>
  Term (Maybe a) ->
  (Term a -> p) ->
  Pred
onJust = onCon @"Just"

-- | `isCon` specialized to `Just`
isJust ::
  forall a.
  (HasSpec a, IsNormalType a) =>
  Term (Maybe a) ->
  Pred
isJust = isCon @"Just"

-- |  ChooseSpec is one of the ways we can 'Or' two Specs together
--    This works for any kind of type that has a HasSpec instance.
--    If your type is a Sum type. One can use CaseOn which is much easier.
chooseSpec ::
  HasSpec a =>
  (Int, Specification a) ->
  (Int, Specification a) ->
  Specification a
chooseSpec (w, s) (w', s') =
  constrained $ \x ->
    exists (\eval -> pure $ if eval x `conformsToSpec` s then PickFirst else PickSecond) $ \p ->
      caseOn
        p
        (branchW w' $ \_ -> (x `satisfies` s))
        (branchW w $ \_ -> (x `satisfies` s'))

data Picky = PickFirst | PickSecond deriving (Ord, Eq, Show, Generic)

instance HasSimpleRep Picky

instance HasSpec Picky

------------------------------------------------------------------------
-- Some generic instances of HasSpec and HasSimpleRep
------------------------------------------------------------------------

instance (Typeable a, Typeable b) => HasSimpleRep (a, b)

instance (Typeable a, Typeable b, Typeable c) => HasSimpleRep (a, b, c)

instance (Typeable a, Typeable b, Typeable c, Typeable d) => HasSimpleRep (a, b, c, d)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => HasSimpleRep (a, b, c, d, e)

instance
  (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable g) =>
  HasSimpleRep (a, b, c, d, e, g)

instance
  (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable g, Typeable h) =>
  HasSimpleRep (a, b, c, d, e, g, h)

instance Typeable a => HasSimpleRep (Maybe a)

instance (Typeable a, Typeable b) => HasSimpleRep (Either a b)

instance
  ( HasSpec a
  , HasSpec b
  ) =>
  HasSpec (a, b)

instance
  ( HasSpec a
  , HasSpec b
  , HasSpec c
  ) =>
  HasSpec (a, b, c)

instance
  ( HasSpec a
  , HasSpec b
  , HasSpec c
  , HasSpec d
  ) =>
  HasSpec (a, b, c, d)

instance
  ( HasSpec a
  , HasSpec b
  , HasSpec c
  , HasSpec d
  , HasSpec e
  ) =>
  HasSpec (a, b, c, d, e)

instance
  ( HasSpec a
  , HasSpec b
  , HasSpec c
  , HasSpec d
  , HasSpec e
  , HasSpec g
  ) =>
  HasSpec (a, b, c, d, e, g)

instance
  ( HasSpec a
  , HasSpec b
  , HasSpec c
  , HasSpec d
  , HasSpec e
  , HasSpec g
  , HasSpec h
  ) =>
  HasSpec (a, b, c, d, e, g, h)

instance
  (IsNormalType a, HasSpec a) =>
  HasSpec (Maybe a)

instance
  ( HasSpec a
  , IsNormalType a
  , HasSpec b
  , IsNormalType b
  ) =>
  HasSpec (Either a b)
