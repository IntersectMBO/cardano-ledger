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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Experiment.Specs.SumProd (
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
  leftFn,
  rightFn,
  prodFst_,
  prodSnd_,
  prod_,
  PairSpec (..),
) where

import Constrained.Core (Evidence (..))
import Constrained.Experiment.Base
import Constrained.Experiment.Conformance (conformsToSpec, satisfies)
import Constrained.Experiment.Generic
import Constrained.Experiment.NumSpec (cardinality)
import Constrained.Experiment.Specs.ListFoldy
import Constrained.Experiment.Syntax (exists, forAll, letBind, mkCase, reify)
import Constrained.Experiment.TheKnot
import Constrained.List
import Constrained.Spec.Pairs ()
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Typeable (Typeable, (:~:) (Refl))
import GHC.TypeLits (Symbol)
import GHC.TypeNats
import Prettyprinter hiding (cat)
import Test.QuickCheck (Arbitrary (..))

-- ========== HasSpec for Prod ==================================

pairView :: forall a b. (HasSpec a, HasSpec b) => Term (Prod a b) -> Maybe (Term a, Term b)
pairView (App (sameFunSym $ PairW @a @b -> Just (_, Refl, Refl, Refl, Refl, Refl)) (x :> y :> Nil)) = Just (x, y)
pairView _ = Nothing

cartesian ::
  forall a b.
  (HasSpec a, HasSpec b) =>
  Specification a ->
  Specification b ->
  Specification (Prod a b)
cartesian (ErrorSpec es) (ErrorSpec fs) = ErrorSpec (es <> fs)
cartesian (ErrorSpec es) _ = ErrorSpec (NE.cons "cartesian left" es)
cartesian _ (ErrorSpec es) = ErrorSpec (NE.cons "cartesian right" es)
cartesian s s' = typeSpec $ Cartesian s s'

data PairSpec a b = Cartesian (Specification a) (Specification b)

instance (Arbitrary (Specification a), Arbitrary (Specification b)) => Arbitrary (PairSpec a b) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  shrink (Cartesian a b) = uncurry Cartesian <$> shrink (a, b)

instance (HasSpec a, HasSpec b) => HasSpec (Prod a b) where
  type TypeSpec (Prod a b) = PairSpec a b

  type Prerequisites (Prod a b) = (HasSpec a, HasSpec b)

  emptySpec = Cartesian mempty mempty

  combineSpec (Cartesian a b) (Cartesian a' b') = cartesian (a <> a') (b <> b')

  conformsTo (Prod a b) (Cartesian sa sb) = conformsToSpec a sa && conformsToSpec b sb

  genFromTypeSpec (Cartesian sa sb) = Prod <$> genFromSpecT sa <*> genFromSpecT sb

  shrinkWithTypeSpec (Cartesian sa sb) (Prod a b) =
    [Prod a' b | a' <- shrinkWithSpec sa a]
      ++ [Prod a b' | b' <- shrinkWithSpec sb b]

  toPreds x (Cartesian sf ss) =
    satisfies (prodFst_ x) sf
      <> satisfies (prodSnd_ x) ss

  cardinalTypeSpec (Cartesian x y) = (cardinality x) + (cardinality y)

  typeSpecHasError (Cartesian x y) =
    case (isErrorLike x, isErrorLike y) of
      (False, False) -> Nothing
      (True, False) -> Just $ errorLikeMessage x
      (False, True) -> Just $ errorLikeMessage y
      (True, True) -> Just $ (errorLikeMessage x <> errorLikeMessage y)

  alternateShow (Cartesian left right@(TypeSpec r [])) =
    case alternateShow @b r of
      (BinaryShow "Cartesian" ps) -> BinaryShow "Cartesian" ("," <+> viaShow left : ps)
      (BinaryShow "SumSpec" ps) -> BinaryShow "Cartesian" ("," <+> viaShow left : ["SumSpec" /> vsep ps])
      _ -> BinaryShow "Cartesian" ["," <+> viaShow left, "," <+> viaShow right]
  alternateShow (Cartesian left right) = BinaryShow "Cartesian" ["," <+> viaShow left, "," <+> viaShow right]

instance (HasSpec a, HasSpec b) => Show (PairSpec a b) where
  show pair@(Cartesian l r) = case alternateShow @(Prod a b) pair of
    (BinaryShow "Cartesian" ps) -> show $ parens ("Cartesian" /> vsep ps)
    _ -> "(Cartesian " ++ "(" ++ show l ++ ") (" ++ show r ++ "))"

{-

p1 :: Term (SimpleRep (Bool,Bool))
p1 = prod_ (Lit True) (Lit False)

p2 :: Term (Bool,Bool)
p2 = fromGeneric_ p1

pp :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (a,b)
pp x y = fromGeneric_ (prod_ x y)

qq x y = toGeneric_ (pp x y)

baz :: Term a -> Maybe String
baz (ToGeneric (FromGeneric x)) = Just $ "BINGO! To From " ++ show x
baz (ToGeneric x) = Just $ "To " ++ show x
baz (Fst (ToGeneric x)) = Just $ "BONGO! Fst To "++show x
baz (Fst x) = Just $ "Fst "++show x
baz (Lit _) = Just $ "Lit "
baz (V _) = Just $ "Var "
baz (App f _) = Just $ "App "++show f
-- baz _ = Nothing

_foo :: (HasSpec a, HasSpec b) => Term(a,b) -> Term a
_foo ab = prodFst_ (toGeneric_ ab)

tuple :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (a,b)
tuple x y = fromGeneric_ (prod_ x y)

-}

{-
pattern Tuple ::
   forall rng. () =>
   forall a b. ( AppRequires () "fromGenericFn" BaseW '[Prod a b] rng
               , AppRequires () "prod_" BaseW [a, b] (Prod a b) ) =>
               -- , HasSimpleRep rng, SimpleRep rng ~ Prod a b,TypeSpec rng ~ PairSpec a b ) =>
   Term a -> Term b -> Term rng
pattern Tuple x y <- FromGeneric (Pair x y)
   -- where Tuple x y =  FromGeneric (Pair x y)
-}

{-
pattern FstTuple ::
  forall c.   () =>
  forall b d. ( AppRequires () "prodFst_" BaseW '[Prod c b] c
                , SimpleRep d ~ Prod c b
                , AppRequires () "toGenericFn" BaseW '[d] (SimpleRep d)
                ) => Term d -> Term c
pattern FstTuple x <- Fst (ToGeneric x)
-}

-- ==================================================
-- FunSym instances for Pairs
-- ==================================================

-- ========= FstW

instance (HasSpec a, HasSpec b) => FunSym () "prodFst_" BaseW '[Prod a b] a where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence FstW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App FstW (v' :> Nil)) (v :-> ps)
  propagate (Context Evidence FstW (HOLE :<> End)) (TypeSpec ts cant) =
    cartesian @a @b (TypeSpec ts cant) TrueSpec
  propagate (Context Evidence FstW (HOLE :<> End)) (MemberSpec es) =
    cartesian @a @b (MemberSpec es) TrueSpec
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for FstW with wrong number of arguments. " ++ show ctx)

  rewriteRules FstW ((pairView -> Just (x, _)) :> Nil) Evidence = Just x
  rewriteRules _ _ _ = Nothing

  mapTypeSpec FstW (Cartesian s _) = s

prodFst_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term a
prodFst_ = appTerm FstW

-- ========= SndW

instance (HasSpec a, HasSpec b) => FunSym () "prodSnd_" BaseW '[Prod a b] b where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence SndW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App SndW (v' :> Nil)) (v :-> ps)
  propagate (Context Evidence SndW (HOLE :<> End)) (TypeSpec ts cant) =
    cartesian @a @b TrueSpec (TypeSpec ts cant)
  propagate (Context Evidence SndW (HOLE :<> End)) (MemberSpec es) =
    cartesian @a @b TrueSpec (MemberSpec es)
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for SndW with wrong number of arguments. " ++ show ctx)

  rewriteRules SndW ((pairView -> Just (_, y)) :> Nil) Evidence = Just y
  rewriteRules _ _ _ = Nothing

  mapTypeSpec SndW (Cartesian _ s) = s

prodSnd_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term b
prodSnd_ = appTerm SndW

-- ========= PairW
sameFst :: Eq a1 => a1 -> [Prod a1 a2] -> [a2]
sameFst a ps = [b | Prod a' b <- ps, a == a']

sameSnd :: Eq a1 => a1 -> [Prod a2 a1] -> [a2]
sameSnd b ps = [a | Prod a b' <- ps, b == b']

instance (HasSpec a, HasSpec b) => FunSym () "prod_" BaseW '[a, b] (Prod a b) where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence PairW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App PairW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context Evidence PairW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App PairW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context Evidence PairW (a :|> HOLE :<> End)) ts@(TypeSpec (Cartesian sa sb) cant)
    | a `conformsToSpec` sa = sb <> foldMap notEqualSpec (sameFst a cant)
    | otherwise =
        ErrorSpec
          (NE.fromList ["propagate (pair_ " ++ show a ++ " HOLE) has conformance failure on a", show ts])
  propagate (Context Evidence PairW (HOLE :<> b :<| End)) ts@(TypeSpec (Cartesian sa sb) cant)
    | b `conformsToSpec` sb = sa <> foldMap notEqualSpec (sameSnd b cant)
    | otherwise =
        ErrorSpec
          (NE.fromList ["propagate (pair_ HOLE " ++ show b ++ ") has conformance failure on b", show ts])
  propagate (Context Evidence PairW (a :|> HOLE :<> End)) (MemberSpec es) =
    case (nub (sameFst a (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show a ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show a ++ " does not appear as the fst component of anything in the MemberSpec."
            ]
  propagate (Context Evidence PairW (HOLE :<> b :<| End)) (MemberSpec es) =
    case (nub (sameSnd b (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show b ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show b ++ " does not appear as the snd component of anything in the MemberSpec."
            ]
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for PairW with wrong number of arguments. " ++ show ctx)

prod_ :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (Prod a b)
prod_ = appTerm PairW

-- ==================================================================
-- The HasSpec instance for Sum is in TheKnot.
-- Here are the FunSym Instances for Sum
-- ===================================================================

-- ============= InjLeftW ====

instance (HasSpec a, HasSpec b, KnownNat (CountCases b)) => FunSym () "leftFn" BaseW '[a] (Sum a b) where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence InjLeftW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App InjLeftW (v' :> Nil)) (v :-> ps)
  propagate (Context Evidence InjLeftW (HOLE :<> End)) (TypeSpec (SumSpec _ sl _) cant) =
    sl <> foldMap notEqualSpec [a | SumLeft a <- cant]
  propagate (Context Evidence InjLeftW (HOLE :<> End)) (MemberSpec es) =
    case [a | SumLeft a <- NE.toList es] of
      (x : xs) -> MemberSpec (x :| xs)
      [] ->
        ErrorSpec $
          pure $
            "propMemberSpec (sumleft_ HOLE) on (MemberSpec es) with no SumLeft in es: " ++ show (NE.toList es)
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for InjLeftW with wrong number of arguments. " ++ show ctx)

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    InjLeftW -> typeSpec $ SumSpec Nothing (typeSpec ts) (ErrorSpec (pure "mapTypeSpec InjLeftW"))

leftFn :: (HasSpec a, HasSpec b, KnownNat (CountCases b)) => Term a -> Term (Sum a b)
leftFn = appTerm InjLeftW

-- ============= InjRightW ====

instance
  (HasSpec a, HasSpec b, KnownNat (CountCases b)) =>
  FunSym () "rightFn" BaseW '[b] (Sum a b)
  where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence InjRightW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App InjRightW (v' :> Nil)) (v :-> ps)
  propagate (Context Evidence InjRightW (HOLE :<> End)) (TypeSpec (SumSpec _ _ sr) cant) =
    sr <> foldMap notEqualSpec [a | SumRight a <- cant]
  propagate (Context Evidence InjRightW (HOLE :<> End)) (MemberSpec es) =
    case [a | SumRight a <- NE.toList es] of
      (x : xs) -> MemberSpec (x :| xs)
      [] ->
        ErrorSpec $
          pure $
            "propagate(InjRight HOLE) on (MemberSpec es) with no SumLeft in es: " ++ show (NE.toList es)
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for InjRightW with wrong number of arguments. " ++ show ctx)

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    InjRightW -> typeSpec $ SumSpec Nothing (ErrorSpec (pure "mapTypeSpec InjRightW")) (typeSpec ts)

rightFn :: (HasSpec a, HasSpec b, KnownNat (CountCases b)) => Term b -> Term (Sum a b)
rightFn = appTerm InjRightW

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
  inj_ = leftFn -- app injLeftFn

instance
  {-# OVERLAPPABLE #-}
  ( HasSpec (ProdOver con)
  , SOPTerm c (con' : sop)
  , ConstrOf c (con' : sop) ~ ConstrOf c ((c' ::: con) : con' : sop)
  , KnownNat (CountCases (SOP (con' : sop)))
  ) =>
  SOPTerm c ((c' ::: con) : con' : sop)
  where
  inj_ = rightFn . inj_ @c @(con' : sop)

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
fst_ = appTerm (ComposeW FstW ToGenericW)

fstW :: (HasSpec a, HasSpec b) => FunW ((), ()) "composeFn" '[(a, b)] a
fstW = (ComposeW FstW ToGenericW)

snd_ :: (HasSpec x, HasSpec y) => Term (x, y) -> Term y
snd_ = appTerm (ComposeW SndW ToGenericW)

sndW :: (HasSpec a, HasSpec b) => FunW ((), ()) "composeFn" '[(a, b)] b
sndW = (ComposeW SndW ToGenericW)

{-
fst_ ::
  forall a b.
  ( HasSpec a
  , HasSpec b
  ) =>
  Term (a, b) ->
  Term a
fst_ = sel @0

snd_ ::
  forall a b.
  ( HasSpec a
  , HasSpec b
  ) =>
  Term (a, b) ->
  Term b
snd_ = sel @1
-}

{-
fst_ ::
  ( HasSpec a
  , HasSpec b
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term (a, b) ->
  Term a
fst_ = prodFst_ . toGeneric_

snd_ ::
  ( HasSpec a
  , HasSpec b
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term (a, b) ->
  Term b
snd_ = prodSnd_ . toGeneric_
-}

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
left_ = fromGeneric_ . leftFn

right_ ::
  ( HasSpec a
  , HasSpec b
  , IsNormalType a
  , IsNormalType b
  ) =>
  Term b ->
  Term (Either a b)
right_ = fromGeneric_ . rightFn

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
