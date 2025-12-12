{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Data.Set.NonEmpty (
  NonEmptySet,
  fromFoldable,
  fromSet,
  singleton,
  toList,
  toSet,
) where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Control.DeepSeq (NFData)
import qualified Data.Foldable as Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import NoThunks.Class (NoThunks)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import qualified Test.QuickCheck as QC

newtype NonEmptySet a = NonEmptySet (Set a)
  deriving stock (Show, Eq)
  deriving newtype (EncCBOR, DecCBOR, NoThunks, NFData)

instance (Arbitrary a, Ord a) => Arbitrary (NonEmptySet a) where
  arbitrary = fmap NonEmptySet (arbitrary `QC.suchThat` (not . null))

  shrink (NonEmptySet xs) =
    [ NonEmptySet xs'
    | xs' <- shrink xs
    , not (Set.null xs')
    ]

-- | \(O(1)\).
singleton :: forall a. a -> NonEmptySet a
singleton = NonEmptySet . Set.singleton

-- | \(O(1)\).
fromSet :: forall a. Set a -> Maybe (NonEmptySet a)
fromSet set = if Set.null set then Nothing else Just (NonEmptySet set)

-- | \(O(1)\).
toSet :: forall a. NonEmptySet a -> Set a
toSet (NonEmptySet set) = set

-- | \(O(n \log n)\).
fromFoldable :: forall f a. (Foldable f, Ord a) => f a -> Maybe (NonEmptySet a)
fromFoldable = fromSet . Foldable.foldl' (flip Set.insert) Set.empty

-- | \(O(n)\).
toList :: forall a. NonEmptySet a -> [a]
toList (NonEmptySet set) = Set.toList set
