{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
module Ledger.Core where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Data.AbstractSize
import Ledger.Signatures

-- | Hash part of the ledger paylod
class HasHash a where
  hash :: a -> Hash

---------------------------------------------------------------------------------
-- Signing and verification
---------------------------------------------------------------------------------

-- |Representation of the owner of key pair.
newtype Owner = Owner Natural deriving (Show, Eq, Ord, HasTypeReps)

class HasOwner a where
  owner :: a -> Owner

-- |Signing Key.
newtype SKey = SKey Owner deriving (Show, Eq, Ord, HasTypeReps)

instance HasOwner SKey where
  owner (SKey o) = o

-- |Verification Key.
newtype VKey = VKey Owner deriving (Show, Eq, Ord, HasTypeReps)

instance HasHash VKey where
  hash = Crypto.hash

instance BA.ByteArrayAccess VKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance HasOwner VKey where
  owner (VKey o) = o

-- | A genesis key is a specialisation of a generic VKey.
newtype VKeyGenesis = VKeyGenesis VKey
  deriving (Eq, Ord, Show, HasTypeReps)

instance HasOwner VKeyGenesis where
  owner (VKeyGenesis vk) = owner vk

-- |Key Pair.
data KeyPair = KeyPair
  {sKey :: SKey, vKey :: VKey} deriving (Show, Eq, Ord, Generic)

instance HasTypeReps KeyPair

-- |Return a key pair for a given owner.
keyPair :: Owner -> KeyPair
keyPair o = KeyPair (SKey o) (VKey o)

-- |A digital signature.
data Sig a = Sig a Owner deriving (Show, Eq, Ord, Generic)

instance HasTypeReps a => HasTypeReps (Sig a)

-- |Produce a digital signature
sign :: SKey -> a -> Sig a
sign (SKey k) d = Sig d k

-- |Verify a digital signature
verify :: Eq a => VKey -> a -> Sig a -> Bool
verify (VKey vk) vd (Sig sd sk) = vk == sk && vd == sd

---------------------------------------------------------------------------------
-- Slots and Epochs
---------------------------------------------------------------------------------

newtype Epoch = Epoch Word64
  deriving (Eq, Ord, Show, HasTypeReps)

newtype Slot = Slot { unSlot :: Word64 }
  deriving (Eq, Ord, Show, HasTypeReps)

-- | A number of slots.
--
--   We use this newtype to distinguish between a cardinal slot and a relative
--   period of slots.
newtype SlotCount = SlotCount Word64
  deriving (Eq, Ord, Num, Show)

-- | Add a slot count to a slot.
addSlot :: Slot -> SlotCount -> Slot
addSlot (Slot n) (SlotCount m) = Slot $ m + n

-- | Subtract a slot count from a slot.
--
--   This is bounded below by 0.
minusSlot :: Slot -> SlotCount -> Slot
minusSlot (Slot m) (SlotCount n)
  | m <= n    = Slot 0
  | otherwise = Slot $ m - n

---------------------------------------------------------------------------------
-- Domain restriction and exclusion
---------------------------------------------------------------------------------

newtype PairSet a b = PairSet {unPairSet :: Set (a,b)}
  deriving (Eq, Show)

psSize :: PairSet a b -> Int
psSize = Set.size . unPairSet

class Maplike m where
  singleton :: a -> b -> m a b

  -- | Domain
  dom :: Ord a => m a b -> Set a
  -- | Range
  range :: Ord b => m a b -> Set b

  -- |Domain restriction
  --
  (◁), (◃) :: Ord a => Set a -> m a b -> m a b

  -- |Domain exclusion
  --
  (⋪) :: Ord a => Set a -> m a b -> m a b

  -- | Range restriction
  --
  (▹) :: Ord b => m a b -> Set b -> m a b

  -- | Union
  (∪) :: (Ord a, Ord b) => m a b -> m a b -> m a b

  -- | Union Override
  (⨃) :: (Ord a, Ord b) => m a b -> m a b -> m a b

instance Maplike Map where
  singleton = Map.singleton

  dom = Map.keysSet
  range = Set.fromList . Map.elems

  s ◁ r = Map.filterWithKey (\k _ -> k `Set.member` s) r
  s ◃ r = s ◁ r

  s ⋪ r = Map.filterWithKey (\k _ -> k `Set.notMember` s) r

  r ▹ s = Map.filter (flip Set.member s) r

  d0 ∪ d1 = Map.union d0 d1
  d0 ⨃ d1 = d1 ∪ (Map.keysSet d1 ⋪ d0)

instance Maplike PairSet where
  singleton a b = PairSet $ Set.singleton (a,b)

  dom = Set.map fst . unPairSet
  range = Set.map snd . unPairSet

  s ◁ r = PairSet . Set.filter (\(k,_) -> k `Set.member` s) $ unPairSet r
  s ◃ r = s ◁ r

  s ⋪ r = PairSet . Set.filter (\(k,_) -> k `Set.notMember` s) $ unPairSet r

  r ▹ s = PairSet . Set.filter (\(_,v) -> Set.member v s) $ unPairSet r

  (PairSet d0) ∪ (PairSet d1) = PairSet $ Set.union d0 d1

  d0 ⨃ d1 = d1 ∪ ((dom d1) ⋪ d0)
