{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Set.NonEmpty (
  NonEmptySet,
  fromFoldable,
  fromSet,
  singleton,
  insert,
  toSet,
  fromNonEmpty,
) where

import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR, decodeSet)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.IsList (IsList (..))
import NoThunks.Class (NoThunks)

newtype NonEmptySet a = NonEmptySet (Set a)
  deriving stock (Show, Eq, Ord)
  deriving newtype (EncCBOR, NoThunks, NFData, ToJSON, Semigroup, Foldable)

instance (Ord a, FromJSON a) => FromJSON (NonEmptySet a) where
  parseJSON v = do
    s <- parseJSON v
    case fromSet s of
      Nothing -> fail "Empty set found, expected non-empty"
      Just nes -> pure nes

instance (Typeable a, Ord a, DecCBOR a) => DecCBOR (NonEmptySet a) where
  decCBOR = do
    set <- decodeSet decCBOR
    case fromSet set of
      Nothing -> fail "Empty set found, expected non-empty"
      Just nes -> pure nes
  {-# INLINE decCBOR #-}

instance Ord a => IsList (NonEmptySet a) where
  type Item (NonEmptySet a) = a

  fromList = fromMaybe (error "NonEmptySet.fromList: empty list") . fromFoldable

  -- \| \(O(n)\).
  toList (NonEmptySet set) = Set.toList set

-- | \(O(1)\).
insert :: Ord a => a -> NonEmptySet a -> NonEmptySet a
insert x (NonEmptySet s) = consSet x s

-- | \(O(1)\)
consSet :: Ord a => a -> Set a -> NonEmptySet a
consSet x s = NonEmptySet $ Set.insert x s

-- | \(O(1)\).
singleton :: a -> NonEmptySet a
singleton = NonEmptySet . Set.singleton

-- | \(O(1)\).
fromSet :: Set a -> Maybe (NonEmptySet a)
fromSet set = if Set.null set then Nothing else Just (NonEmptySet set)

-- | \(O(1)\).
toSet :: NonEmptySet a -> Set a
toSet (NonEmptySet set) = set

-- | \(O(n \log n)\).
fromFoldable :: (Foldable f, Ord a) => f a -> Maybe (NonEmptySet a)
fromFoldable = fromSet . Foldable.foldl' (flip Set.insert) Set.empty

-- | \(O(n \log n)\)
fromNonEmpty :: Ord a => NonEmpty a -> NonEmptySet a
fromNonEmpty (x :| xs) = consSet x $ Set.fromList xs
