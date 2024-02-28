{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Spec.Generics (
  GenericsFn,
  IsNormalType,
  fst_,
  snd_,
  pair_,
  left_,
  right_,
  caseOn,
  branch,
  forAll',
  constrained',
  con,
  match,
  onJust,
) where

import Constrained.Base
import Constrained.Core
import Constrained.List
import Constrained.Spec.Pairs ()
import Constrained.Univ

------------------------------------------------------------------------
-- Generics
------------------------------------------------------------------------

instance FunctionLike (GenericsFn fn) where
  sem = \case
    ToGeneric -> toSimpleRep
    FromGeneric -> fromSimpleRep

instance BaseUniverse fn => Functions (GenericsFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    _
      | SuspendedSpec v ps <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \v' ->
            let args = appendList (mapList (\(Value a) -> Lit a) pre) (v' :> mapList (\(Value a) -> Lit a) suf)
             in Let (App (injectFn fn) args) (v :-> ps)
    ToGeneric | NilCtx HOLE <- ctx -> case spec of
      TypeSpec s cant -> TypeSpec s (fromSimpleRep <$> cant)
      MemberSpec es -> MemberSpec (fromSimpleRep <$> es)
    FromGeneric | NilCtx HOLE <- ctx -> case spec of
      TypeSpec s cant -> TypeSpec s (toSimpleRep <$> cant)
      MemberSpec es -> MemberSpec (toSimpleRep <$> es)

  rewriteRules f@FromGeneric =
    -- No TypeAbstractions in ghc-8.10
    case f of
      (_ :: GenericsFn fn '[SimpleRep a] a) ->
        [rewriteRule_ $ \x -> fromGeneric_ (toGeneric_ x) ~> x]
  rewriteRules f@ToGeneric =
    -- No TypeAbstractions in ghc-8.10
    case f of
      (_ :: GenericsFn fn '[a] (SimpleRep a)) ->
        [rewriteRule_ $ \x -> toGeneric_ (fromGeneric_ @a x) ~> x]

  mapTypeSpec f ts = case f of
    ToGeneric -> typeSpec ts
    FromGeneric -> typeSpec ts

-- HasSpec for various generic types --------------------------------------

instance HasSimpleRep (a, b)
instance HasSimpleRep (a, b, c)
instance HasSimpleRep (a, b, c, d)
instance HasSimpleRep (a, b, c, d, e)
instance HasSimpleRep (a, b, c, d, e, g)
instance HasSimpleRep (a, b, c, d, e, g, h)
instance HasSimpleRep (Maybe a)
instance HasSimpleRep (Either a b)

instance
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  HasSpec fn (a, b)
instance
  ( HasSpec fn a
  , HasSpec fn b
  , HasSpec fn c
  ) =>
  HasSpec fn (a, b, c)
instance
  ( HasSpec fn a
  , HasSpec fn b
  , HasSpec fn c
  , HasSpec fn d
  ) =>
  HasSpec fn (a, b, c, d)
instance
  ( HasSpec fn a
  , HasSpec fn b
  , HasSpec fn c
  , HasSpec fn d
  , HasSpec fn e
  ) =>
  HasSpec fn (a, b, c, d, e)
instance
  ( HasSpec fn a
  , HasSpec fn b
  , HasSpec fn c
  , HasSpec fn d
  , HasSpec fn e
  , HasSpec fn g
  ) =>
  HasSpec fn (a, b, c, d, e, g)
instance
  ( HasSpec fn a
  , HasSpec fn b
  , HasSpec fn c
  , HasSpec fn d
  , HasSpec fn e
  , HasSpec fn g
  , HasSpec fn h
  ) =>
  HasSpec fn (a, b, c, d, e, g, h)
instance
  HasSpec fn a =>
  HasSpec fn (Maybe a)
instance
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  HasSpec fn (Either a b)

-- HasSpec for () ---------------------------------------------------------

instance BaseUniverse fn => HasSpec fn () where
  type TypeSpec fn () = ()
  emptySpec = ()
  combineSpec _ _ = typeSpec ()
  _ `conformsTo` _ = True
  genFromTypeSpec _ = pure ()
  toPreds _ _ = TruePred

------------------------------------------------------------------------
-- Sums
------------------------------------------------------------------------

instance BaseUniverse fn => Functions (SumFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    _
      | SuspendedSpec v ps <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \v' ->
            let args = appendList (mapList (\(Value a) -> Lit a) pre) (v' :> mapList (\(Value a) -> Lit a) suf)
             in Let (App (injectFn fn) args) (v :-> ps)
    InjLeft | NilCtx HOLE <- ctx -> case spec of
      TypeSpec (SumSpec sl _) cant -> sl <> foldMap notEqualSpec [a | SumLeft a <- cant]
      MemberSpec es -> MemberSpec [a | SumLeft a <- es]
    InjRight | NilCtx HOLE <- ctx -> case spec of
      TypeSpec (SumSpec _ sr) cant -> sr <> foldMap notEqualSpec [a | SumRight a <- cant]
      MemberSpec es -> MemberSpec [a | SumRight a <- es]

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    InjLeft -> typeSpec $ SumSpec (typeSpec ts) (ErrorSpec ["mapTypeSpec InjLeft"])
    InjRight -> typeSpec $ SumSpec (ErrorSpec ["mapTypeSpec InjRight"]) (typeSpec ts)

------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------

fst_ ::
  forall fn a b.
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  Term fn (a, b) ->
  Term fn a
fst_ = app fstFn . toGeneric_

snd_ ::
  forall fn a b.
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  Term fn (a, b) ->
  Term fn b
snd_ = app sndFn . toGeneric_

pair_ ::
  forall fn a b.
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  Term fn a ->
  Term fn b ->
  Term fn (a, b)
pair_ a b = fromGeneric_ $ app pairFn a b

left_ ::
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  Term fn a ->
  Term fn (Either a b)
left_ = fromGeneric_ . app injLeftFn

right_ ::
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  Term fn b ->
  Term fn (Either a b)
right_ = fromGeneric_ . app injRightFn

caseOn ::
  forall fn a.
  ( HasSpec fn a
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , SimpleRep a ~ SumOver (Cases (SimpleRep a))
  , TypeList (Cases (SimpleRep a))
  ) =>
  Term fn a ->
  FunTy (MapList (Binder fn) (Cases (SimpleRep a))) (Pred fn)
caseOn tm = curryList @(Cases (SimpleRep a)) (mkCase (toGeneric_ tm))

branch ::
  forall fn p a.
  ( HasSpec fn a
  , All (HasSpec fn) (Args a)
  , IsPred p fn
  , IsProd a
  ) =>
  FunTy (MapList (Term fn) (Args a)) p ->
  Binder fn a
branch body =
  -- NOTE: It's not sufficient to simply apply `body` to all the arguments
  -- with `uncurryList` because that will mean that `var` is repeated in the
  -- body. For example, consider `branch $ \ i j -> i <=. j`. If we don't
  -- build the lets this will boil down to `p :-> fst p <=. snd p` which
  -- will blow up at generation time. If we instead do: `p :-> Let x (fst p) (Let y (snd p) (x <=. y))`
  -- the solver will solve `x` and `y` separately (`y` before `x` in this case) and things
  -- will work just fine.
  bind (buildBranch @p @fn body . toArgs @a @fn)

match ::
  forall fn p a.
  ( HasSpec fn a
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , Cases (SimpleRep a) ~ '[SimpleRep a]
  , SimpleRep a ~ SumOver (Cases (SimpleRep a))
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , IsProd (SimpleRep a)
  , All (HasSpec fn) (Args (SimpleRep a))
  , IsPred p fn
  ) =>
  Term fn a ->
  FunTy (MapList (Term fn) (Args (SimpleRep a))) p ->
  Pred fn
match p m = caseOn p (branch @fn @p m)

buildBranch ::
  forall p fn as.
  ( All (HasSpec fn) as
  , IsPred p fn
  , BaseUniverse fn
  ) =>
  FunTy (MapList (Term fn) as) p ->
  List (Term fn) as ->
  Pred fn
buildBranch bd Nil = toPred bd
buildBranch bd (t :> args) =
  letBind t $ \x -> buildBranch @p @fn (bd x) args

type family Args t where
  Args (Prod a b) = a : Args b
  Args a = '[a]

class IsProd p where
  toArgs ::
    ( HasSpec fn p
    , BaseUniverse fn
    ) =>
    Term fn p ->
    List (Term fn) (Args p)

instance {-# OVERLAPPABLE #-} Args a ~ '[a] => IsProd a where
  toArgs = (:> Nil)

instance IsProd b => IsProd (Prod a b) where
  toArgs (p :: Term fn (Prod a b))
    | Evidence <- prerequisites @fn @(Prod a b) = (app fstFn p) :> toArgs (app sndFn p)

type family ResultType t where
  ResultType (a -> b) = ResultType b
  ResultType a = a

-- NOTE: `ResultType r ~ Term fn a` is NOT a redundant constraint,
-- removing it causes type inference to break elsewhere
con ::
  forall c a r fn.
  ( SimpleRep a ~ SOP (TheSop a)
  , TypeSpec fn a ~ TypeSpec fn (SOP (TheSop a))
  , TypeList (ConstrOf c (TheSop a))
  , HasSpec fn a
  , HasSimpleRep a
  , r ~ FunTy (MapList (Term fn) (ConstrOf c (TheSop a))) (Term fn a)
  , ResultType r ~ Term fn a
  , SOPTerm c fn (TheSop a)
  , ConstrTerm fn (ConstrOf c (TheSop a))
  ) =>
  r
con = curryList @(ConstrOf c (TheSop a)) @(Term fn) (app (fromGenericFn @_ @a) . inj_ @c @fn @(TheSop a) . prod_)

type family Cases t where
  Cases (Sum a b) = a : Cases b
  Cases a = '[a]

forAll' ::
  forall fn t a p.
  ( Forallable t a
  , Cases (SimpleRep a) ~ '[SimpleRep a]
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , HasSpec fn t
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , All (HasSpec fn) (Args (SimpleRep a))
  , IsPred p fn
  , IsProd (SimpleRep a)
  , HasSpec fn a
  ) =>
  Term fn t ->
  FunTy (MapList (Term fn) (Args (SimpleRep a))) p ->
  Pred fn
forAll' xs f = forAll xs $ \x -> match @fn @p x f

constrained' ::
  forall fn a p.
  ( Cases (SimpleRep a) ~ '[SimpleRep a]
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , All (HasSpec fn) (Args (SimpleRep a))
  , IsProd (SimpleRep a)
  , HasSpec fn a
  , IsPred p fn
  ) =>
  FunTy (MapList (Term fn) (Args (SimpleRep a))) p ->
  Spec fn a
constrained' f = constrained $ \x -> match @fn @p x f

class (BaseUniverse fn, HasSpec fn (SOP sop)) => SOPTerm c fn sop where
  inj_ :: Term fn (ProdOver (ConstrOf c sop)) -> Term fn (SOP sop)

instance HasSpec fn (ProdOver constr) => SOPTerm c fn (c ::: constr : '[]) where
  inj_ = id

instance
  ( HasSpec fn (SOP (con : sop))
  , HasSpec fn (ProdOver constr)
  ) =>
  SOPTerm c fn (c ::: constr : con : sop)
  where
  inj_ = app injLeftFn

instance
  {-# OVERLAPPABLE #-}
  ( HasSpec fn (ProdOver con)
  , SOPTerm c fn (con' : sop)
  , ConstrOf c (con' : sop) ~ ConstrOf c ((c' ::: con) : con' : sop)
  ) =>
  SOPTerm c fn ((c' ::: con) : con' : sop)
  where
  inj_ = app injRightFn . inj_ @c @fn @(con' : sop)

class (BaseUniverse fn, HasSpec fn (ProdOver constr)) => ConstrTerm fn constr where
  prod_ :: List (Term fn) constr -> Term fn (ProdOver constr)

instance HasSpec fn a => ConstrTerm fn '[a] where
  prod_ (a :> Nil) = a

instance
  ( HasSpec fn a
  , HasSpec fn (ProdOver (a : b : as))
  , ConstrTerm fn (b : as)
  ) =>
  ConstrTerm fn (a : b : as)
  where
  prod_ (a :> as) = app pairFn a (prod_ as)

type IsNormalType a = (Cases a ~ '[a], Args a ~ '[a], IsProd a)

onJust ::
  (BaseUniverse fn, HasSpec fn a, IsNormalType a, IsPred p fn) =>
  Term fn (Maybe a) ->
  (Term fn a -> p) ->
  Pred fn
onJust tm p = caseOn tm (branch $ const True) (branch p)
