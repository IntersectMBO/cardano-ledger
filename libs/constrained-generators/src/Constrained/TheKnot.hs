{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-name-shadowing #-}

-- | All the things that are mutually recursive.
module Constrained.TheKnot (
  FunW (..),
  ProdW (..),
  SizeW (..),
  PairSpec (..),
  ifElse,
  sizeOf_,

  -- * Useful internal function symbols
  prodFst_,
  prodSnd_,
  prod_,

  -- * Misc
  genFromSizeSpec,
  maxSpec,
  rangeSize,
  hasSize,
  genInverse,
  mapSpec,
  between,

  -- * Patterns
  pattern Product,

  -- * Classes
  Sized (..),
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.FunctionSymbol
import Constrained.GenT
import Constrained.Generation
import Constrained.Generic
import Constrained.List
import Constrained.NumOrd
import Constrained.PrettyUtils
import Constrained.SumList
-- TODO: some strange things here, why is SolverStage in here?!
-- Because it is mutually recursive with something else in here.
import Constrained.Syntax
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Kind
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Typeable
import Prettyprinter hiding (cat)
import Prelude hiding (cycle, pred)

instance Numeric a => Complete a where
  simplifyA = simplifySpec
  genFromSpecA = genFromSpecT

-- | If the `Specification Bool` doesn't constrain the boolean you will get a `TrueSpec` out.
ifElse :: (IsPred p, IsPred q) => Term Bool -> p -> q -> Pred
ifElse b p q = whenTrue b p <> whenTrue (not_ b) q

-- --------------- Simplification of Sum types --------------------

-- =======================================================================================

-- | Functor like property for Specification, but instead of a Haskell function (a -> b),
--   it takes a function symbol (t '[a] b) from a to b.
--   Note, in this context, a function symbol is some constructor of a witnesstype.
--   Eg. ProdFstW, InjRightW, SingletonW, etc. NOT the lifted versions like fst_ singleton_,
--   which construct Terms. We had to wait until here to define this because it
--   depends on Semigroup property of Specification, and Asserting equality
mapSpec ::
  forall t a b.
  AppRequires t '[a] b =>
  t '[a] b ->
  Specification a ->
  Specification b
mapSpec f (ExplainSpec es s) = explainSpec es (mapSpec f s)
mapSpec f TrueSpec = mapTypeSpec f (emptySpec @a)
mapSpec _ (ErrorSpec err) = ErrorSpec err
mapSpec f (MemberSpec as) = MemberSpec $ NE.nub $ fmap (semantics f) as
mapSpec f (SuspendedSpec x p) =
  constrained $ \x' ->
    Exists (\_ -> fatalError "mapSpec") (x :-> fold [Assert $ (x' ==. appTerm f (V x)), p])
mapSpec f (TypeSpec ts cant) = mapTypeSpec f ts <> notMemberSpec (map (semantics f) cant)

-- ================================================================
-- HasSpec for Products
-- ================================================================

pairView :: Term (Prod a b) -> Maybe (Term a, Term b)
pairView (App (getWitness -> Just ProdW) (x :> y :> Nil)) = Just (x, y)
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

-- | t`TypeSpec` for @`Prod` a b@
data PairSpec a b = Cartesian (Specification a) (Specification b)

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

-- ==================================================
-- Logic instances for Prod
-- ==================================================

-- | Function symbols for talking about `Prod`
data ProdW :: [Type] -> Type -> Type where
  ProdW :: (HasSpec a, HasSpec b) => ProdW '[a, b] (Prod a b)
  ProdFstW :: (HasSpec a, HasSpec b) => ProdW '[Prod a b] a
  ProdSndW :: (HasSpec a, HasSpec b) => ProdW '[Prod a b] b

deriving instance Eq (ProdW as b)

deriving instance Show (ProdW as b)

instance Syntax ProdW

instance Semantics ProdW where
  semantics ProdW = Prod
  semantics ProdFstW = prodFst
  semantics ProdSndW = prodSnd

instance Logic ProdW where
  propagateTypeSpec ProdFstW (Unary HOLE) ts cant = cartesian (TypeSpec ts cant) TrueSpec
  propagateTypeSpec ProdSndW (Unary HOLE) ts cant =
    cartesian TrueSpec (TypeSpec ts cant)
  propagateTypeSpec ProdW (a :>: HOLE) sc@(Cartesian sa sb) cant
    | a `conformsToSpec` sa = sb <> foldMap notEqualSpec (sameFst a cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ " ++ show a ++ " HOLE) has conformance failure on a", show (TypeSpec sc cant)]
          )
  propagateTypeSpec ProdW (HOLE :<: b) sc@(Cartesian sa sb) cant
    | b `conformsToSpec` sb = sa <> foldMap notEqualSpec (sameSnd b cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ HOLE " ++ show b ++ ") has conformance failure on b", show (TypeSpec sc cant)]
          )

  propagateMemberSpec ProdFstW (Unary HOLE) es = cartesian (MemberSpec es) TrueSpec
  propagateMemberSpec ProdSndW (Unary HOLE) es = cartesian TrueSpec (MemberSpec es)
  propagateMemberSpec ProdW (a :>: HOLE) es =
    case (nub (sameFst a (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show a ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show a ++ " does not appear as the fst component of anything in the MemberSpec."
            ]
  propagateMemberSpec ProdW (HOLE :<: b) es =
    case (nub (sameSnd b (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show b ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show b ++ " does not appear as the snd component of anything in the MemberSpec."
            ]

  rewriteRules ProdFstW ((pairView -> Just (x, _)) :> Nil) Evidence = Just x
  rewriteRules ProdSndW ((pairView -> Just (_, y)) :> Nil) Evidence = Just y
  rewriteRules _ _ _ = Nothing

  mapTypeSpec ProdFstW (Cartesian s _) = s
  mapTypeSpec ProdSndW (Cartesian _ s) = s

-- | `fst` on `Prod`
prodFst_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term a
prodFst_ = appTerm ProdFstW

-- | `snd` on `Prod`
prodSnd_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term b
prodSnd_ = appTerm ProdSndW

-- | `(,)` on `Prod`
prod_ :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (Prod a b)
prod_ = appTerm ProdW

sameFst :: Eq a1 => a1 -> [Prod a1 a2] -> [a2]
sameFst a ps = [b | Prod a' b <- ps, a == a']

sameSnd :: Eq a1 => a1 -> [Prod a2 a1] -> [a2]
sameSnd b ps = [a | Prod a b' <- ps, b == b']

-- | Pattern for `prod_`
pattern Product ::
  forall c.
  () =>
  forall a b.
  ( c ~ Prod a b
  , AppRequires ProdW '[a, b] (Prod a b)
  ) =>
  Term a ->
  Term b ->
  Term c
pattern Product x y <- (App (getWitness -> Just ProdW) (x :> y :> Nil))

-- ================================================================
-- The TypeSpec for List. Used in the HasSpec instance for Lists
-- ================================================================

-- | Generalized `length` function
sizeOf_ :: (HasSpec a, Sized a) => Term a -> Term Integer
sizeOf_ = curryList (App SizeOfW)

-- | Because Sizes should always be >= 0, We provide this alternate generator
--   that can be used to replace (genFromSpecT @Integer), to ensure this important property
genFromSizeSpec :: MonadGenError m => Specification Integer -> GenT m Integer
genFromSizeSpec integerSpec = genFromSpecT (integerSpec <> geqSpec 0)

-- =====================================================================
-- Syntax, Semantics and Logic instances for function symbols on List

-- ==============  Helper functions

-- ================
-- Sized
-- ================

type SizeSpec = NumSpec Integer

-- | The things we need to talk about the `sizeOf_` a thing
class Sized t where
  sizeOf :: t -> Integer
  default sizeOf :: (HasSimpleRep t, Sized (SimpleRep t)) => t -> Integer
  sizeOf = sizeOf . toSimpleRep

  liftSizeSpec :: HasSpec t => SizeSpec -> [Integer] -> Specification t
  default liftSizeSpec ::
    ( Sized (SimpleRep t)
    , GenericRequires t
    ) =>
    SizeSpec ->
    [Integer] ->
    Specification t
  liftSizeSpec sz cant = fromSimpleRepSpec $ liftSizeSpec sz cant

  liftMemberSpec :: HasSpec t => [Integer] -> Specification t
  default liftMemberSpec ::
    ( Sized (SimpleRep t)
    , GenericRequires t
    ) =>
    [Integer] ->
    Specification t
  liftMemberSpec = fromSimpleRepSpec . liftMemberSpec

  sizeOfTypeSpec :: HasSpec t => TypeSpec t -> Specification Integer
  default sizeOfTypeSpec ::
    ( HasSpec (SimpleRep t)
    , Sized (SimpleRep t)
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    ) =>
    TypeSpec t ->
    Specification Integer
  sizeOfTypeSpec = sizeOfTypeSpec @(SimpleRep t)

-- =============================================================
-- All Foldy class instances are over Numbers (so far).
-- Foldy class requires higher order functions, so here they are.
-- Note this is a new witness type, different from BaseW
-- but serving the same purpose. Note it can take Witnesses from
-- other classes as inputs. See ComposeW
-- ==============================================================

-- | Function symbols for basic higher-order functions
data FunW (dom :: [Type]) (rng :: Type) where
  IdW :: forall a. FunW '[a] a
  ComposeW ::
    forall b t1 t2 a r.
    ( AppRequires t1 '[b] r
    , AppRequires t2 '[a] b
    , HasSpec b
    ) =>
    t1 '[b] r ->
    t2 '[a] b ->
    FunW '[a] r

instance Semantics FunW where
  semantics IdW = id
  semantics (ComposeW f g) = semantics f . semantics g

instance Syntax FunW

instance Show (FunW dom rng) where
  show IdW = "id_"
  show (ComposeW x y) = "(compose_ " ++ show x ++ " " ++ show y ++ ")"

instance Eq (FunW dom rng) where
  IdW == IdW = True
  ComposeW f f' == ComposeW g g' = compareWit f g && compareWit f' g'
  _ == _ = False

compareWit ::
  forall t1 bs1 r1 t2 bs2 r2.
  (AppRequires t1 bs1 r1, AppRequires t2 bs2 r2) =>
  t1 bs1 r1 ->
  t2 bs2 r2 ->
  Bool
compareWit x y = case (eqT @t1 @t2, eqT @bs1 @bs2, eqT @r1 @r2) of
  (Just Refl, Just Refl, Just Refl) -> x == y
  _ -> False

-- ===================================
-- Logic instances for IdW and ComposeW

instance Logic FunW where
  propagate IdW (Unary HOLE) = id
  propagate (ComposeW f g) (Unary HOLE) = propagate g (Unary HOLE) . propagate f (Unary HOLE)

  mapTypeSpec IdW ts = typeSpec ts
  mapTypeSpec (ComposeW g h) ts = mapSpec g . mapSpec h $ typeSpec ts

  -- Note we need the Evidence to apply App to f, and to apply App to g
  rewriteRules (ComposeW f g) (x :> Nil) Evidence = Just $ App f (App g (x :> Nil) :> Nil)
  rewriteRules IdW (x :> Nil) Evidence = Just x

-- =======================================================
-- The Foldy class instances for Numbers
-- =======================================================

-- | Invert a `Fun` and combine it with a `Specification` for the input to
-- generate a value
genInverse ::
  ( MonadGenError m
  , HasSpec a
  , HasSpec b
  ) =>
  Fun '[a] b ->
  Specification a ->
  b ->
  GenT m a
genInverse (Fun f) argS x =
  let argSpec' = argS <> propagate f (HOLE :? Nil) (equalSpec x)
   in explainNE
        ( NE.fromList
            [ "genInverse"
            , "  f = " ++ show f
            , show $ "  argS =" <+> pretty argS
            , "  x = " ++ show x
            , show $ "  argSpec' =" <+> pretty argSpec'
            ]
        )
        $ genFromSpecT argSpec'

-- | Function symbols for generalized `length` and `Data.Set.size` functions.
-- Used to implement `sizeOf_`.
data SizeW (dom :: [Type]) rng :: Type where
  SizeOfW :: (Sized n, HasSpec n) => SizeW '[n] Integer

deriving instance Eq (SizeW ds r)

instance Show (SizeW d r) where
  show SizeOfW = "sizeOf_"

instance Semantics SizeW where
  semantics SizeOfW = sizeOf -- From the Sized class.

instance Syntax SizeW

instance Logic SizeW where
  propagateTypeSpec SizeOfW (Unary HOLE) ts cant = liftSizeSpec ts cant

  propagateMemberSpec SizeOfW (Unary HOLE) es = liftMemberSpec (NE.toList es)

  mapTypeSpec (SizeOfW :: SizeW '[a] b) ts =
    constrained $ \x ->
      unsafeExists $ \x' -> Assert (x ==. sizeOf_ x') <> toPreds @a x' ts

-- ======================================

-- | A spec for a positive non-empty range
rangeSize :: Integer -> Integer -> SizeSpec
rangeSize a b | a < 0 || b < 0 = error ("Negative Int in call to rangeSize: " ++ show a ++ " " ++ show b)
rangeSize a b = NumSpecInterval (Just a) (Just b)

-- | Constrain a number to be between two points
between :: (HasSpec a, TypeSpec a ~ NumSpec a) => a -> a -> Specification a
between lo hi = TypeSpec (NumSpecInterval (Just lo) (Just hi)) []

-- | The widest interval whose largest element is admitted by the original spec
maxSpec :: Specification Integer -> Specification Integer
maxSpec (ExplainSpec es s) = explainSpec es (maxSpec s)
maxSpec TrueSpec = TrueSpec
maxSpec s@(SuspendedSpec _ _) =
  constrained $ \x -> unsafeExists $ \y -> [y `satisfies` s, Explain (pure "maxSpec on SuspendedSpec") $ Assert (x <=. y)]
maxSpec (ErrorSpec xs) = ErrorSpec xs
maxSpec (MemberSpec xs) = leqSpec (maximum xs)
maxSpec (TypeSpec (NumSpecInterval _ hi) bad) = TypeSpec (NumSpecInterval Nothing hi) bad

-- | How to constrain the size of any type, with a Sized instance
hasSize :: (HasSpec t, Sized t) => SizeSpec -> Specification t
hasSize sz = liftSizeSpec sz []
