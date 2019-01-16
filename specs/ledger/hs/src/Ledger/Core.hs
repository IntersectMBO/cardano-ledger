module Ledger.Core where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Natural (minusNaturalMaybe)
import Numeric.Natural (Natural)

import Ledger.Signatures

-- | Hash part of the ledger paylod
class HasHash a where
  hash :: a -> Hash

---------------------------------------------------------------------------------
-- Signing and verification
---------------------------------------------------------------------------------

-- |Representation of the owner of key pair.
newtype Owner = Owner Natural deriving (Show, Eq, Ord)

class HasOwner a where
  owner :: a -> Owner

-- |Signing Key.
newtype SKey = SKey Owner deriving (Show, Eq, Ord)

instance HasOwner SKey where
  owner (SKey o) = o

-- |Verification Key.
newtype VKey = VKey Owner deriving (Show, Eq, Ord)

instance HasHash VKey where
  hash = Crypto.hash

instance BA.ByteArrayAccess VKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance HasOwner VKey where
  owner (VKey o) = o

-- | A genesis key is a specialisation of a generic VKey.
newtype VKeyGenesis = VKeyGenesis VKey
  deriving (Eq, Ord, Show)

instance HasOwner VKeyGenesis where
  owner (VKeyGenesis vk) = owner vk

-- |Key Pair.
data KeyPair = KeyPair
  {sKey :: SKey, vKey :: VKey} deriving (Show, Eq, Ord)

-- |Return a key pair for a given owner.
keyPair :: Owner -> KeyPair
keyPair o = KeyPair (SKey o) (VKey o)

-- |A digital signature.
data Sig a = Sig a Owner deriving (Show, Eq, Ord)

-- |Produce a digital signature
sign :: SKey -> a -> Sig a
sign (SKey k) d = Sig d k

-- |Verify a digital signature
verify :: Eq a => VKey -> a -> Sig a -> Bool
verify (VKey vk) vd (Sig sd sk) = vk == sk && vd == sd

---------------------------------------------------------------------------------
-- Slots and Epochs
---------------------------------------------------------------------------------

newtype Epoch = Epoch Natural
  deriving (Eq, Ord, Show)

newtype Slot = Slot Natural
  deriving (Eq, Ord, Show)

-- | A number of slots.
--
--   We use this newtype to distinguish between a cardinal slot and a relative
--   period of slots.
newtype SlotCount = SlotCount Natural
  deriving (Eq, Ord, Show)

-- | Add a slot count to a slot.
addSlot :: Slot -> SlotCount -> Slot
addSlot (Slot n) (SlotCount m) = Slot $ m + n

-- | Subtract a slot count from a slot.
--
--   This is bounded below by 0.
minusSlot :: Slot -> SlotCount -> Slot
minusSlot (Slot n) (SlotCount m) = case minusNaturalMaybe m n of
  Nothing -> Slot 0
  Just k  -> Slot k

---------------------------------------------------------------------------------
-- Domain restriction and exclusion
---------------------------------------------------------------------------------

-- |Domain restriction
--
(◁) :: Ord a => Set a -> Map a b -> Map a b
s ◁ r = Map.filterWithKey (\k _ -> k `Set.member` s) r

-- |Domain exclusion
--
(⋪) :: Ord a => Set a -> Map a b -> Map a b
s ⋪ r = Map.filterWithKey (\k _ -> k `Set.notMember` s) r

-- | Union Override
(⨃) :: Ord a => Map a b -> Map a b -> Map a b
d0 ⨃ d1 = d1 `Map.union` (Map.keysSet d1 ⋪ d0)
