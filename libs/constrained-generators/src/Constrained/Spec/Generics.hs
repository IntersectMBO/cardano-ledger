{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
  ifElse,
  pickBool,
  chooseSpec,
) where

import Data.Typeable
import GHC.TypeLits
import Test.QuickCheck (Arbitrary (..), oneof)

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

  rewriteRules FromGeneric (App (extractFn @(GenericsFn fn) @fn -> Just ToGeneric) (x :> Nil) :> _) = cast x
  rewriteRules ToGeneric (App (extractFn @(GenericsFn fn) @fn -> Just FromGeneric) (x :> Nil) :> _) = Just x
  rewriteRules _ _ = Nothing

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
  (IsNormalType a, HasSpec fn a) =>
  HasSpec fn (Maybe a)
instance
  ( HasSpec fn a
  , IsNormalType a
  , HasSpec fn b
  , IsNormalType b
  ) =>
  HasSpec fn (Either a b)

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
      TypeSpec (SumSpec _ sl _) cant -> sl <> foldMap notEqualSpec [a | SumLeft a <- cant]
      MemberSpec es -> MemberSpec [a | SumLeft a <- es]
    InjRight | NilCtx HOLE <- ctx -> case spec of
      TypeSpec (SumSpec _ _ sr) cant -> sr <> foldMap notEqualSpec [a | SumRight a <- cant]
      MemberSpec es -> MemberSpec [a | SumRight a <- es]

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    -- TODO possibly not the right counts??
    InjLeft -> typeSpec $ SumSpec Nothing (typeSpec ts) (ErrorSpec ["mapTypeSpec InjLeft"])
    InjRight -> typeSpec $ SumSpec Nothing (ErrorSpec ["mapTypeSpec InjRight"]) (typeSpec ts)

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
fst_ = sel @0

snd_ ::
  forall fn a b.
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  Term fn (a, b) ->
  Term fn b
snd_ = sel @1

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
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term fn a ->
  Term fn (Either a b)
left_ = fromGeneric_ . app injLeftFn

right_ ::
  ( HasSpec fn a
  , HasSpec fn b
  , IsNormalType a
  , IsNormalType b
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
  , SimpleRep a ~ SumOver (FlattenSum (SimpleRep a))
  , TypeList (FlattenSum (SimpleRep a))
  ) =>
  Term fn a ->
  FunTy (MapList (Weighted (Binder fn)) (FlattenSum (SimpleRep a))) (Pred fn)
caseOn tm = curryList @(FlattenSum (SimpleRep a)) (mkCase (toGeneric_ tm))

branch ::
  forall fn p a.
  ( HasSpec fn a
  , All (HasSpec fn) (FlattenProd a)
  , IsPred p fn
  , IsProd a
  ) =>
  FunTy (MapList (Term fn) (FlattenProd a)) p ->
  Weighted (Binder fn) a
branch body =
  -- NOTE: It's not sufficient to simply apply `body` to all the arguments
  -- with `uncurryList` because that will mean that `var` is repeated in the
  -- body. For example, consider `branch $ \ i j -> i <=. j`. If we don't
  -- build the lets this will boil down to `p :-> fst p <=. snd p` which
  -- will blow up at generation time. If we instead do: `p :-> Let x (fst p) (Let y (snd p) (x <=. y))`
  -- the solver will solve `x` and `y` separately (`y` before `x` in this case) and things
  -- will work just fine.
  Weighted Nothing (bind (buildBranch @p @fn body . toFlattenProd @a @fn))

branchW ::
  forall fn p a.
  ( HasSpec fn a
  , All (HasSpec fn) (FlattenProd a)
  , IsPred p fn
  , IsProd a
  ) =>
  Int ->
  FunTy (MapList (Term fn) (FlattenProd a)) p ->
  Weighted (Binder fn) a
branchW w body =
  Weighted (Just w) (bind (buildBranch @p @fn body . toFlattenProd @a @fn))

match ::
  forall fn p a.
  ( HasSpec fn a
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , FlattenSum (SimpleRep a) ~ '[SimpleRep a]
  , SimpleRep a ~ SumOver (FlattenSum (SimpleRep a))
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , IsProd (SimpleRep a)
  , All (HasSpec fn) (FlattenProd (SimpleRep a))
  , IsPred p fn
  ) =>
  Term fn a ->
  FunTy (MapList (Term fn) (FlattenProd (SimpleRep a))) p ->
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

type family FlattenProd t where
  FlattenProd (Prod a b) = a : FlattenProd b
  FlattenProd a = '[a]

class IsProd p where
  toFlattenProd ::
    ( HasSpec fn p
    , BaseUniverse fn
    ) =>
    Term fn p ->
    List (Term fn) (FlattenProd p)

instance {-# OVERLAPPABLE #-} FlattenProd a ~ '[a] => IsProd a where
  toFlattenProd = (:> Nil)

instance IsProd b => IsProd (Prod a b) where
  toFlattenProd (p :: Term fn (Prod a b))
    | Evidence <- prerequisites @fn @(Prod a b) = (app fstFn p) :> toFlattenProd (app sndFn p)

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
con =
  curryList @(ConstrOf c (TheSop a)) @(Term fn)
    (app (fromGenericFn @_ @a) . inj_ @c @fn @(TheSop a) . prod_)

sel ::
  forall n fn a c as.
  ( SimpleRep a ~ ProdOver as
  , TheSop a ~ '[c ::: as]
  , TypeSpec fn a ~ TypeSpec fn (ProdOver as)
  , Select fn n as
  , HasSpec fn a
  , HasSpec fn (ProdOver as)
  , HasSimpleRep a
  ) =>
  Term fn a ->
  Term fn (At n as)
sel = select_ @fn @n @as . toGeneric_

type family FlattenSum t where
  FlattenSum (Sum a b) = a : FlattenSum b
  FlattenSum a = '[a]

-- | Like `forAll` but pattern matches on the `Term fn a`
forAll' ::
  forall fn t a p.
  ( Forallable t a
  , FlattenSum (SimpleRep a) ~ '[SimpleRep a]
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , HasSpec fn t
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , All (HasSpec fn) (FlattenProd (SimpleRep a))
  , IsPred p fn
  , IsProd (SimpleRep a)
  , HasSpec fn a
  ) =>
  Term fn t ->
  FunTy (MapList (Term fn) (FlattenProd (SimpleRep a))) p ->
  Pred fn
forAll' xs f = forAll xs $ \x -> match @fn @p x f

-- | Like `constrained` but pattern matches on the bound `Term fn a`
constrained' ::
  forall a fn p.
  ( FlattenSum (SimpleRep a) ~ '[SimpleRep a]
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , All (HasSpec fn) (FlattenProd (SimpleRep a))
  , IsProd (SimpleRep a)
  , HasSpec fn a
  , IsPred p fn
  ) =>
  FunTy (MapList (Term fn) (FlattenProd (SimpleRep a))) p ->
  Specification fn a
constrained' f = constrained $ \x -> match @fn @p x f

-- | Like `reify` but pattern matches on the bound `Term fn b`
reify' ::
  forall fn a b p.
  ( FlattenSum (SimpleRep b) ~ '[SimpleRep b]
  , TypeSpec fn b ~ TypeSpec fn (SimpleRep b)
  , HasSpec fn (SimpleRep b)
  , HasSimpleRep b
  , All (HasSpec fn) (FlattenProd (SimpleRep b))
  , IsProd (SimpleRep b)
  , HasSpec fn a
  , HasSpec fn b
  , IsPred p fn
  ) =>
  Term fn a ->
  (a -> b) ->
  FunTy (MapList (Term fn) (FlattenProd (SimpleRep b))) p ->
  Pred fn
reify' a r f = reify a r $ \x -> match @fn @p x f

class (BaseUniverse fn, HasSpec fn (SOP sop)) => SOPTerm c fn sop where
  inj_ :: Term fn (ProdOver (ConstrOf c sop)) -> Term fn (SOP sop)

instance HasSpec fn (ProdOver constr) => SOPTerm c fn (c ::: constr : '[]) where
  inj_ = id

instance
  ( HasSpec fn (SOP (con : sop))
  , HasSpec fn (ProdOver constr)
  , KnownNat (CountCases (SOP (con : sop)))
  ) =>
  SOPTerm c fn (c ::: constr : con : sop)
  where
  inj_ = app injLeftFn

instance
  {-# OVERLAPPABLE #-}
  ( HasSpec fn (ProdOver con)
  , SOPTerm c fn (con' : sop)
  , ConstrOf c (con' : sop) ~ ConstrOf c ((c' ::: con) : con' : sop)
  , KnownNat (CountCases (SOP (con' : sop)))
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

type family At n as where
  At 0 (a : as) = a
  At n (a : as) = At (n - 1) as

class Select fn n as where
  select_ :: Term fn (ProdOver as) -> Term fn (At n as)

instance Select fn 0 (a : '[]) where
  select_ = id

instance (HasSpec fn a, HasSpec fn (ProdOver (a' : as))) => Select fn 0 (a : a' : as) where
  select_ = app fstFn

instance
  {-# OVERLAPPABLE #-}
  ( HasSpec fn a
  , HasSpec fn (ProdOver (a' : as))
  , At (n - 1) (a' : as) ~ At n (a : a' : as)
  , Select fn (n - 1) (a' : as)
  ) =>
  Select fn n (a : a' : as)
  where
  select_ = select_ @fn @(n - 1) @(a' : as) . app sndFn

class IsConstrOf (c :: Symbol) b sop where
  mkCases ::
    (HasSpec fn b, All (HasSpec fn) (FlattenSum (SOP sop))) =>
    (forall a. Term fn a -> Pred fn) ->
    (Term fn b -> Pred fn) ->
    List (Weighted (Binder fn)) (FlattenSum (SOP sop))

instance
  ( b ~ ProdOver as
  , TypeList (FlattenSum (SOP (con : sop)))
  ) =>
  IsConstrOf c b ((c ::: as) : con : sop)
  where
  mkCases r (k :: Term fn b -> Pred fn) =
    Weighted Nothing (bind k)
      :> mapListC @(HasSpec fn) (\_ -> Weighted Nothing (bind r)) (listShape @(FlattenSum (SOP (con : sop))))

instance
  ( b ~ ProdOver as
  , IsNormalType b
  ) =>
  IsConstrOf c b '[c ::: as]
  where
  mkCases _ (k :: Term fn b -> Pred fn) = Weighted Nothing (bind k) :> Nil

instance
  {-# OVERLAPPABLE #-}
  ( FlattenSum (SOP ((c' ::: as) : cs)) ~ (ProdOver as : FlattenSum (SOP cs))
  , IsConstrOf c b cs
  ) =>
  IsConstrOf c b ((c' ::: as) : cs)
  where
  mkCases r k = Weighted Nothing (bind (r @(ProdOver as))) :> mkCases @c @_ @cs r k

-- TODO: the constraints around this are horrible!! We should figure out a way to make these things nicer.
onCon ::
  forall c a fn p.
  ( IsConstrOf c (ProdOver (ConstrOf c (TheSop a))) (TheSop a)
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , HasSimpleRep a
  , HasSpec fn a
  , HasSpec fn (SimpleRep a)
  , SumOver (FlattenSum (SOP (TheSop a))) ~ SimpleRep a
  , All (HasSpec fn) (FlattenSum (SOP (TheSop a)))
  , HasSpec fn (ProdOver (ConstrOf c (TheSop a)))
  , IsPred p fn
  , FlattenProd (ProdOver (ConstrOf c (TheSop a))) ~ ConstrOf c (TheSop a)
  , All (HasSpec fn) (ConstrOf c (TheSop a))
  , IsProd (ProdOver (ConstrOf c (TheSop a)))
  ) =>
  Term fn a ->
  FunTy (MapList (Term fn) (ConstrOf c (TheSop a))) p ->
  Pred fn
onCon tm p =
  Case
    (toGeneric_ tm)
    ( mkCases @c @(ProdOver (ConstrOf c (TheSop a))) @(TheSop a)
        (const $ assert True)
        (buildBranch @p p . toFlattenProd)
    )

isCon ::
  forall c a fn.
  ( IsConstrOf c (ProdOver (ConstrOf c (TheSop a))) (TheSop a)
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , HasSimpleRep a
  , HasSpec fn a
  , HasSpec fn (SimpleRep a)
  , SumOver (FlattenSum (SOP (TheSop a))) ~ SimpleRep a
  , All (HasSpec fn) (FlattenSum (SOP (TheSop a)))
  , HasSpec fn (ProdOver (ConstrOf c (TheSop a)))
  ) =>
  Term fn a ->
  Pred fn
isCon tm =
  Case
    (toGeneric_ tm)
    ( mkCases @c @(ProdOver (ConstrOf c (TheSop a))) @(TheSop a)
        (const $ assert False)
        (const $ assert True)
    )

type IsNormalType a = (FlattenSum a ~ '[a], FlattenProd a ~ '[a], IsProd a, CountCases a ~ 1)

onJust ::
  forall fn a p.
  (BaseUniverse fn, HasSpec fn a, IsNormalType a, IsPred p fn) =>
  Term fn (Maybe a) ->
  (Term fn a -> p) ->
  Pred fn
onJust = onCon @"Just"

isJust ::
  forall fn a.
  (BaseUniverse fn, HasSpec fn a, IsNormalType a) =>
  Term fn (Maybe a) ->
  Pred fn
isJust = isCon @"Just"

pickBool :: BaseUniverse fn => Int -> Int -> Specification fn Bool
pickBool w w' = constrained $ \ b ->
  caseOn b (branchW w' $ const True) (branchW w $ const True)

chooseSpec ::
  HasSpec fn a =>
  (Int, Specification fn a) ->
  (Int, Specification fn a) ->
  Specification fn a
chooseSpec (w, s) (w', s') =
  constrained $ \x ->
    exists (\eval -> pure $ eval x `conformsToSpec` s) $ \b ->
      [ ifElse
          b
          (x `satisfies` s)
          (x `satisfies` s')
      , b `satisfies` pickBool w w'
      ]

-- Arbitrary instances ----------------------------------------------------

instance
  (HasSpec fn a, HasSpec fn b, Arbitrary (FoldSpec fn a), Arbitrary (FoldSpec fn b)) =>
  Arbitrary (FoldSpec fn (a, b))
  where
  arbitrary =
    oneof
      [ preMapFoldSpec (composeFn fstFn toGenericFn) <$> arbitrary
      , preMapFoldSpec (composeFn sndFn toGenericFn) <$> arbitrary
      , pure NoFold
      ]
  shrink NoFold = []
  shrink FoldSpec {} = [NoFold]
