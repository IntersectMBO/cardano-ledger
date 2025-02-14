{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Experiment.Specs.Size where

import Constrained.Experiment.Base
import Constrained.Experiment.Conformance (satisfies)
import Constrained.Experiment.Generic
import Constrained.Experiment.NumSpec
import Constrained.Experiment.Syntax
import Constrained.Experiment.TheKnot

import Constrained.Core
import Constrained.GenT
import Data.Kind
import qualified Data.List.NonEmpty as NE
import GHC.TypeLits

-- ======================================================================
-- Size and its 'generic' operations over Sized types.
-- ======================================================================

-- | Because Sizes should always be >= 0, We provide this alternate generator
--   that can be used to replace (genFromSpecT @Integer), to ensure this important property
genFromSizeSpec :: MonadGenError m => Specification Integer -> GenT m Integer
genFromSizeSpec integerSpec = genFromSpecT (integerSpec <> geqSpec 0)

data SizeW (c :: Constraint) (s :: Symbol) (dom :: [Type]) rng :: Type where
  SizeOfW :: forall n. SizeW (Sized n) "sizeOf_" '[n] Integer

deriving instance Eq (SizeW c s ds r)

instance Show (SizeW c s d r) where
  show SizeOfW = "sizeOf_"

instance Witness SizeW where
  semantics SizeOfW = sizeOf -- From the Sized class.

instance HasSpec t => FunSym (Sized t) "sizeOf_" SizeW '[t] Integer where
 
  propTypeSpec  (Context Evidence SizeOfW (HOLE :<> End)) ts cant = liftSizeSpec ts cant
  propTypeSpec ctx _ _ = ErrorSpec (NE.fromList ["SizeOfW[sizeOf_] on TypeSpec ", "Unreachable context, wrong number of args", show ctx])

  propMemberSpec  (Context Evidence SizeOfW (HOLE :<> End)) es = liftMemberSpec (NE.toList es)
  propMemberSpec ctx _ = ErrorSpec (NE.fromList ["SizeOfW[sizeOf_] on MemberSpec ", "Unreachable context, wrong number of args", show ctx])

  mapTypeSpec f ts = mapTypeSpecSize f ts

mapTypeSpecSize ::
  forall a b t.
  (HasSpec a, Sized t) => SizeW (Sized t) "sizeOf_" '[a] b -> TypeSpec a -> Specification b
mapTypeSpecSize SizeOfW ts =
  constrained $ \x ->
    unsafeExists $ \x' ->
      Assert (Equal x (sizeOf_ x')) <> toPreds @t x' ts

sizeOfFn :: forall a. (HasSpec a, Sized a) => Fun '[a] Integer
sizeOfFn = Fun Evidence SizeOfW

sizeOf_ :: (HasSpec t, Sized t) => Term t -> Term Integer
sizeOf_ = appTerm SizeOfW

-- ======================================

rangeSize :: Integer -> Integer -> SizeSpec
rangeSize a b | a < 0 || b < 0 = error ("Negative Int in call to rangeSize: " ++ show a ++ " " ++ show b)
rangeSize a b = NumSpecInterval (Just a) (Just b)

between :: (HasSpec a, TypeSpec a ~ NumSpec a) => a -> a -> Specification a
between lo hi = TypeSpec (NumSpecInterval (Just lo) (Just hi)) []

-- | The widest interval whose largest element is admitted by the original spec
maxSpec :: Specification Integer -> Specification Integer
maxSpec (ExplainSpec es s) = explainSpecOpt es (maxSpec s)
maxSpec TrueSpec = TrueSpec
maxSpec s@(SuspendedSpec _ _) =
  constrained $ \x -> unsafeExists $ \y -> [y `satisfies` s, Explain (pure "maxSpec on SuspendedSpec") $ Assert (x <=. y)]
maxSpec (ErrorSpec xs) = ErrorSpec xs
maxSpec (MemberSpec xs) = leqSpec (maximum xs)
maxSpec (TypeSpec (NumSpecInterval _ hi) bad) = TypeSpec (NumSpecInterval Nothing hi) bad

-- ================
-- Sized
-- ================

type SizeSpec = NumSpec Integer

class Sized t where
  sizeOf :: t -> Integer
  default sizeOf :: (HasSimpleRep t, Sized (SimpleRep t)) => t -> Integer
  sizeOf = sizeOf . toSimpleRep

  liftSizeSpec :: HasSpec t => SizeSpec -> [Integer] -> Specification t
  default liftSizeSpec ::
    ( HasSpec t
    , HasSimpleRep t
    , Sized (SimpleRep t)
    , HasSpec (SimpleRep t)
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    ) =>
    SizeSpec -> [Integer] -> Specification t
  liftSizeSpec sz cant = fromSimpleRepSpec $ liftSizeSpec sz cant

  liftMemberSpec :: HasSpec t => [Integer] -> Specification t
  default liftMemberSpec ::
    ( HasSpec t
    , HasSpec (SimpleRep t)
    , HasSimpleRep t
    , Sized (SimpleRep t)
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    ) =>
    [Integer] -> Specification t
  liftMemberSpec = fromSimpleRepSpec . liftMemberSpec

  sizeOfTypeSpec :: HasSpec t => TypeSpec t -> Specification Integer
  default sizeOfTypeSpec ::
    ( HasSpec (SimpleRep t)
    , Sized (SimpleRep t)
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    ) =>
    TypeSpec t -> Specification Integer
  sizeOfTypeSpec = sizeOfTypeSpec @(SimpleRep t)

-- How to constrain the size of any type, with a Sized instance
hasSize :: (HasSpec t, Sized t) => SizeSpec -> Specification t
hasSize sz = liftSizeSpec sz []
