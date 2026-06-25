{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Map.NonEmpty (
  NonEmptyMap,
  fromFoldable,
  fromMap,
  singleton,
  toList,
  toMap,
) where

import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR)
import Control.DeepSeq (NFData)
import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import NoThunks.Class (NoThunks)
import Prelude hiding (map)

newtype NonEmptyMap k v = NonEmptyMap (Map k v)
  deriving stock (Show, Eq)
  deriving newtype (EncCBOR, NoThunks, NFData)

instance (Ord k, DecCBOR k, DecCBOR v) => DecCBOR (NonEmptyMap k v) where
  decCBOR = do
    m <- decCBOR
    case fromMap m of
      Nothing -> fail "Empty map found, expected non-empty"
      Just nem -> pure nem
  {-# INLINE decCBOR #-}

-- | \(O(1)\).
singleton :: forall k v. k -> v -> NonEmptyMap k v
singleton k v = NonEmptyMap $ Map.singleton k v

-- | \(O(1)\).
fromMap :: forall k v. Map k v -> Maybe (NonEmptyMap k v)
fromMap x = if Map.null x then Nothing else Just (NonEmptyMap x)

-- | \(O(1)\).
toMap :: forall k v. NonEmptyMap k v -> Map k v
toMap (NonEmptyMap set) = set

-- | \(O(n \log n)\).
fromFoldable :: forall f k v. (Foldable f, Ord k) => f (k, v) -> Maybe (NonEmptyMap k v)
fromFoldable = fromMap . Foldable.foldl' (flip (uncurry Map.insert)) Map.empty

-- | \(O(n)\).
toList :: forall k v. NonEmptyMap k v -> [(k, v)]
toList (NonEmptyMap x) = Map.toList x
