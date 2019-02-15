{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
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

-- | Subtract a slot from a slot count.
--
--   This is bounded below by 0.
minusSlot :: SlotCount -> Slot -> Slot
minusSlot (SlotCount m) (Slot n)
  | m <= n    = Slot 0
  | otherwise = Slot $ m - n

---------------------------------------------------------------------------------
-- Domain restriction and exclusion
---------------------------------------------------------------------------------

-- |Domain restriction
--
(◁), (◃) :: Ord a => Set a -> Map a b -> Map a b
s ◁ r = Map.filterWithKey (\k _ -> k `Set.member` s) r
s ◃ r = s ◁ r

-- |Domain exclusion
--
(⋪) :: Ord a => Set a -> Map a b -> Map a b
s ⋪ r = Map.filterWithKey (\k _ -> k `Set.notMember` s) r


-- | Range restriction
--
(▹) :: Ord b => Map a b -> Set b -> Map a b
r ▹ s = Map.filter (flip Set.member s) r

-- | Union Override
(⨃) :: Ord a => Map a b -> Map a b -> Map a b
d0 ⨃ d1 = d1 `Map.union` (Map.keysSet d1 ⋪ d0)
