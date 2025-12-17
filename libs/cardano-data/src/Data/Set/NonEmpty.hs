{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Set.NonEmpty (
  NonEmptySet,
  fromFoldable,
  fromSet,
  singleton,
  toList,
  toSet,
) where

import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR, decodeSet)
import Control.DeepSeq (NFData)
import qualified Data.Foldable as Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks)

newtype NonEmptySet a = NonEmptySet (Set a)
  deriving stock (Show, Eq)
  deriving newtype (EncCBOR, NoThunks, NFData)

instance (Typeable a, Ord a, DecCBOR a) => DecCBOR (NonEmptySet a) where
  decCBOR = do
    set <- decodeSet decCBOR
    case fromSet set of
      Nothing -> fail "Empty set found, expected non-empty"
      Just nes -> pure nes
  {-# INLINE decCBOR #-}

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

-- | \(O(n)\).
toList :: NonEmptySet a -> [a]
toList (NonEmptySet set) = Set.toList set
