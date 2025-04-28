{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Spec.SumProd (
  IsNormalType,
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
  cJust_,
  cNothing_,
  fst_,
  snd_,
  fstW,
  sndW,
  pair_,
  injLeft_,
  injRight_,
  prodFst_,
  prodSnd_,
  prod_,
  PairSpec (..),
  SumW (..),
) where

import Constrained.Base (
  BaseW (..),
  Binder (..),
  Forallable (..),
  Fun (..),
  HasSpec (..),
  IsPred (..),
  Pred (..),
  Specification (..),
  Term (..),
  Weighted (..),
  appTerm,
  bind,
  constrained,
  fromGeneric_,
  toGeneric_,
 )
import Constrained.Conformance (
  conformsToSpec,
  satisfies,
 )
import Constrained.Core (
  Evidence (..),
 )
import Constrained.Generic (
  ConstrOf,
  HasSimpleRep (..),
  Prod,
  ProdOver,
  SOP,
  SimpleRep,
  Sum,
  SumOver,
  TheSop,
  (:::),
 )
import Constrained.List (
  All,
  FunTy,
  List (..),
  MapList,
  TypeList,
  curryList,
  listShape,
  mapListC,
 )
import Constrained.Syntax (
  exists,
  forAll,
  letBind,
  mkCase,
  reify,
 )
import Constrained.TheKnot (
  CountCases,
  FoldSpec (..),
  FunW (..),
  PairSpec (..),
  ProdW (..),
  SumW (..),
  ifElse,
  injLeft_,
  injRight_,
  preMapFoldSpec,
  prodFst_,
  prodSnd_,
  prod_,
 )

import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)
import GHC.TypeNats
import Test.QuickCheck (Arbitrary (..), oneof)

-- ==================================================================
-- Generics
-- HasSpec for various types that are Sums of Products
-- ==================================================================

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
    | Evidence <- prerequisites @(Prod a b) = (prodFst_ p) :> toArgs (prodSnd_ p)

type family Args t where
  Args (Prod a b) = a : Args b
  Args a = '[a]

type family ResultType t where
  ResultType (a -> b) = ResultType b
  ResultType a = a

type IsNormalType a = (Cases a ~ '[a], Args a ~ '[a], IsProd a, CountCases a ~ 1)

type family Cases t where
  Cases (Sum a b) = a : Cases b
  Cases a = '[a]

type IsProductType a =
  ( HasSimpleRep a
  , Cases (SimpleRep a) ~ '[SimpleRep a]
  , SimpleRep a ~ SumOver (Cases (SimpleRep a))
  , IsProd (SimpleRep a)
  , HasSpec (SimpleRep a)
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , All HasSpec (ProductAsList a)
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

------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------

fst_ :: (HasSpec x, HasSpec y) => Term (x, y) -> Term x
fst_ = appTerm (ComposeW ProdFstW ToGenericW)

snd_ :: (HasSpec x, HasSpec y) => Term (x, y) -> Term y
snd_ = appTerm (ComposeW ProdSndW ToGenericW)

fstW :: (HasSpec a, HasSpec b) => FunW '[(a, b)] a
fstW = (ComposeW ProdFstW ToGenericW)

sndW :: (HasSpec a, HasSpec b) => FunW '[(a, b)] b
sndW = (ComposeW ProdSndW ToGenericW)

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

left_ ::
  ( HasSpec a
  , HasSpec b
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term a ->
  Term (Either a b)
left_ = fromGeneric_ . injLeft_

right_ ::
  ( HasSpec a
  , HasSpec b
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term b ->
  Term (Either a b)
right_ = fromGeneric_ . injRight_

caseOn ::
  forall a.
  ( HasSpec a
  , HasSpec (SimpleRep a)
  , HasSimpleRep a
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , SimpleRep a ~ SumOver (Cases (SimpleRep a))
  , TypeList (Cases (SimpleRep a))
  ) =>
  Term a ->
  FunTy (MapList (Weighted Binder) (Cases (SimpleRep a))) Pred
caseOn tm = curryList @(Cases (SimpleRep a)) (mkCase (toGeneric_ tm))

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

match ::
  forall p a.
  ( HasSpec a
  , IsProductType a
  , IsPred p
  ) =>
  Term a ->
  FunTy (MapList Term (ProductAsList a)) p ->
  Pred
match p m = caseOn p (branch @p m)

-- NOTE: `ResultType r ~ Term a` is NOT a redundant constraint,
-- removing it causes type inference to break elsewhere
con ::
  forall c a r.
  ( SimpleRep a ~ SOP (TheSop a)
  , TypeSpec a ~ TypeSpec (SOP (TheSop a))
  , TypeList (ConstrOf c (TheSop a))
  , HasSpec a
  , HasSimpleRep a
  , r ~ FunTy (MapList Term (ConstrOf c (TheSop a))) (Term a)
  , ResultType r ~ Term a
  , SOPTerm c (TheSop a)
  , ConstrTerm (ConstrOf c (TheSop a))
  ) =>
  r
con =
  curryList @(ConstrOf c (TheSop a)) @Term
    (fromGeneric_ @a . inj_ @c @(TheSop a) . prodOver_)

cJust_ :: (HasSpec a, IsNormalType a) => Term a -> Term (Maybe a)
cJust_ = con @"Just"

cNothing_ :: (HasSpec a, IsNormalType a) => Term (Maybe a)
cNothing_ = con @"Nothing" (Lit ())

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
  , HasSpec a
  ) =>
  Term t ->
  FunTy (MapList Term (Args (SimpleRep a))) p ->
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
  , IsPred p
  ) =>
  FunTy (MapList Term (Args (SimpleRep a))) p ->
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
  , IsPred p
  ) =>
  Term a ->
  (a -> b) ->
  FunTy (MapList Term (Args (SimpleRep b))) p ->
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
onCon ::
  forall c a p.
  ( IsConstrOf c (ProdOver (ConstrOf c (TheSop a))) (TheSop a)
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , HasSimpleRep a
  , HasSpec a
  , HasSpec (SimpleRep a)
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

isCon ::
  forall c a.
  ( IsConstrOf c (ProdOver (ConstrOf c (TheSop a))) (TheSop a)
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , HasSimpleRep a
  , HasSpec a
  , HasSpec (SimpleRep a)
  , SumOver (Cases (SOP (TheSop a))) ~ SimpleRep a
  , All HasSpec (Cases (SOP (TheSop a)))
  , HasSpec (ProdOver (ConstrOf c (TheSop a)))
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

onJust ::
  forall a p.
  (HasSpec a, IsNormalType a, IsPred p) =>
  Term (Maybe a) ->
  (Term a -> p) ->
  Pred
onJust = onCon @"Just"

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
    exists (\eval -> pure $ eval x `conformsToSpec` s) $ \b ->
      [ ifElse
          b
          (x `satisfies` s)
          (x `satisfies` s')
      , caseOn
          b
          (branchW w' $ \_ -> True)
          (branchW w $ \_ -> True)
      ]
