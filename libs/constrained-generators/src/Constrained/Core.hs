{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- Arbitrary NonEmpty
-- TOOD: fixme by bumping QuickCheck to 3.0 when it's released
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is a collection of relatively core concepts that are re-used
-- throughout the codebase.
module Constrained.Core (
  -- * Variables and renaming
  Var (..),
  eqVar,
  Rename (..),
  freshen,

  -- * Random cruft
  Value (..),
  unValue,
  NonEmpty ((:|)),
  Evidence (..),
  unionWithMaybe,
) where

import Constrained.List (
  List (..),
  mapList,
 )
import Constrained.PrettyUtils
import Control.Applicative
import Data.Function
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Test.QuickCheck (Arbitrary (..), NonEmptyList (NonEmpty))

-- Variables --------------------------------------------------------------

-- | Typed, optionally named, variables
data Var a = Var {nameOf :: Int, nameHint :: String}

instance Ord (Var a) where
  compare = compare `on` nameOf

instance Eq (Var a) where
  (==) = (==) `on` nameOf

instance Show (Var a) where
  show v = nameHint v ++ "_" ++ show (nameOf v)

-- | Check if two variables of different type are equal
eqVar :: forall a a'. (Typeable a, Typeable a') => Var a -> Var a' -> Maybe (a :~: a')
eqVar v v' | nameOf v == nameOf v' = eqT @a @a'
eqVar _ _ = Nothing

-- Variable renaming ------------------------------------------------------

-- | Things where variables can be renamed
class Rename a where
  rename :: Typeable x => Var x -> Var x -> a -> a

instance Typeable a => Rename (Var a) where
  rename v v' vOld
    | Just Refl <- eqVar v vOld = v'
    | otherwise = vOld

instance Rename () where
  rename _ _ _ = ()

instance (Rename a, Rename b) => Rename (a, b) where
  rename x x' (a, b) = (rename x x' a, rename x x' b)

instance {-# OVERLAPPABLE #-} (Functor t, Rename a) => Rename (t a) where
  rename v v'
    | v == v' = id
    | otherwise = fmap (rename v v')

instance (Ord a, Rename a) => Rename (Set a) where
  rename v v'
    | v == v' = id
    | otherwise = Set.map (rename v v')

instance (forall a. Rename (f a)) => Rename (List f as) where
  rename v v' = mapList (rename v v')

instance Rename a => Rename [a] where
  rename v v' = map (rename v v')

freshVar :: Var a -> Set Int -> Var a
freshVar (Var n nh) ns
  | Set.member n ns = Var (1 + Set.findMax ns) nh
  | otherwise = Var n nh

-- | Freshen a variable and rename it in a term where it is used given a set of
-- used names that we can't overlap with
freshen :: (Typeable a, Rename t) => Var a -> t -> Set Int -> (Var a, t)
freshen v t nms
  | nameOf v `Set.member` nms = let v' = freshVar v nms in (v', rename v v' t)
  | otherwise = (v, t)

-- Values -----------------------------------------------------------------

-- | Simple values that we can show
data Value a where
  Value :: Show a => !a -> Value a

deriving instance Eq a => Eq (Value a)

deriving instance Ord a => Ord (Value a)

instance Show (Value a) where
  showsPrec p (Value a) = showsPrec p a

-- | Extract an underlying value from a t`Value`
unValue :: Value a -> a
unValue (Value v) = v

-- Cruft ------------------------------------------------------------------

-- | Evidence that a constraint it satisfied, a runtime dict
data Evidence c where
  Evidence :: c => Evidence c

instance Typeable c => Show (Evidence c) where
  show _ = "Evidence@(" ++ showType @c ++ ")"

-- | Take the union of two `Maybe` values with a given union operator
unionWithMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWithMaybe f ma ma' = (f <$> ma <*> ma') <|> ma <|> ma'

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    NonEmpty xs <- arbitrary
    pure (NE.fromList xs)
