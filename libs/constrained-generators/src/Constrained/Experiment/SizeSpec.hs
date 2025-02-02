{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Random Natural, Arbitrary Natural, Uniform Natural
{-# OPTIONS_GHC -Wno-orphans #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.SizeSpecExperiment where

import Constrained.BaseExperiment

import Constrained.Core (unionWithMaybe)
import Constrained.GenT (GenT, MonadGenError (..), pureGen, sizeT)
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.Foldable (fold)
import Data.Kind
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Typeable (typeOf)
import Data.Word
import GHC.Int
import GHC.Natural
import GHC.Real
import GHC.TypeLits (Symbol)
import System.Random.Stateful (Random (..), Uniform (..))
import Test.QuickCheck (Arbitrary (arbitrary, shrink), choose, frequency)

-- ================
-- Sized
-- ================

class Sized fn t where
  sizeOf :: t -> Integer
  default sizeOf :: (HasSimpleRep t, Sized fn (SimpleRep t)) => t -> Integer
  sizeOf = sizeOf @fn . toSimpleRep

  liftSizeSpec :: HasSpec fn t => SizeSpec fn -> [Integer] -> Specification fn t
  default liftSizeSpec ::
    ( HasSpec fn t
    , HasSimpleRep t
    , Sized fn (SimpleRep t)
    , HasSpec fn (SimpleRep t)
    , TypeSpec fn t ~ TypeSpec fn (SimpleRep t)
    ) =>
    SizeSpec fn -> [Integer] -> Specification fn t
  liftSizeSpec sz cant = fromSimpleRepSpec $ liftSizeSpec sz cant

  liftMemberSpec :: HasSpec fn t => OrdSet Integer -> Specification fn t
  default liftMemberSpec ::
    ( HasSpec fn t
    , HasSpec fn (SimpleRep t)
    , HasSimpleRep t
    , Sized fn (SimpleRep t)
    , TypeSpec fn t ~ TypeSpec fn (SimpleRep t)
    ) =>
    OrdSet Integer -> Specification fn t
  liftMemberSpec = fromSimpleRepSpec . liftMemberSpec

  sizeOfTypeSpec :: HasSpec fn t => TypeSpec fn t -> Specification fn Integer
  default sizeOfTypeSpec ::
    ( HasSpec fn (SimpleRep t)
    , Sized fn (SimpleRep t)
    , TypeSpec fn t ~ TypeSpec fn (SimpleRep t)
    ) =>
    TypeSpec fn t -> Specification fn Integer
  sizeOfTypeSpec = sizeOfTypeSpec @fn @(SimpleRep t)

instance Ord a => Sized fn (Set.Set a) where
  sizeOf = toInteger . Set.size
  liftSizeSpec spec cant = typeSpec (SetSpec mempty TrueSpec (TypeSpec spec cant))
  liftMemberSpec xs = case NE.nonEmpty xs of
    Nothing -> ErrorSpec (pure ("In liftMemberSpec for the (Sized Set) instance, xs is the empty list"))
    Just zs -> typeSpec (SetSpec mempty TrueSpec (MemberSpec zs))
  sizeOfTypeSpec (SetSpec must _ sz) = sz <> geqSpec (sizeOf must)

instance Sized fn [a] where
  sizeOf = toInteger . length
  liftSizeSpec spec cant = typeSpec (ListSpec Nothing mempty (TypeSpec spec cant) TrueSpec NoFold)
  liftMemberSpec xs = case NE.nonEmpty xs of
    Nothing -> ErrorSpec (pure ("In liftMemberSpec for (Sized List) instance, xs is the empty list"))
    Just zs -> typeSpec (ListSpec Nothing mempty (MemberSpec zs) TrueSpec NoFold)
  sizeOfTypeSpec (ListSpec _ _ _ ErrorSpec {} _) = equalSpec 0
  sizeOfTypeSpec (ListSpec _ must sizespec _ _) = sizespec <> geqSpec (sizeOf must)

-- How to constrain the size of any type, with a Sized instance
hasSize :: (HasSpec fn t, Sized fn t) => SizeSpec fn -> Specification fn t
hasSize sz = liftSizeSpec sz []
