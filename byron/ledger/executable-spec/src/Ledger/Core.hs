{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ledger.Core where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum(..))
import Data.Set (Set, isSubsetOf)
import qualified Data.Set as Set
import Data.Word (Word64)
import Data.Foldable (toList, elem)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Data.AbstractSize


-- | An encoded hash of part of the system.
newtype Hash = Hash
  { unHash :: Int
  } deriving stock (Show)
    deriving newtype (Eq, Ord, Hashable, HasTypeReps)

-- | Hash part of the ledger payload
class HasHash a where
  hash :: a -> Hash

---------------------------------------------------------------------------------
-- Signing and verification
---------------------------------------------------------------------------------

-- |Representation of the owner of key pair.
newtype Owner = Owner
  { unOwner :: Natural
  } deriving stock (Show)
    deriving newtype (Eq, Ord, Hashable, HasTypeReps)

class HasOwner a where
  owner :: a -> Owner

-- |Signing Key.
newtype SKey = SKey Owner
  deriving stock (Show)
  deriving newtype (Eq, Ord, HasTypeReps)

instance HasOwner SKey where
  owner (SKey o) = o

-- |Verification Key.
newtype VKey = VKey Owner
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, HasTypeReps)

instance HasHash VKey where
  hash = Hash . H.hash

instance HasOwner VKey where
  owner (VKey o) = o

-- | A genesis key is a specialisation of a generic VKey.
newtype VKeyGenesis = VKeyGenesis VKey
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, HasTypeReps)

instance HasOwner VKeyGenesis where
  owner (VKeyGenesis vk) = owner vk

-- |Key Pair.
data KeyPair = KeyPair
  { sKey :: SKey
  , vKey :: VKey
  } deriving (Eq, Ord, Show, Generic)

instance HasTypeReps KeyPair

-- |Return a key pair for a given owner.
keyPair :: Owner -> KeyPair
keyPair o = KeyPair (SKey o) (VKey o)

-- |A digital signature.
data Sig a = Sig a Owner deriving (Show, Eq, Ord, Generic, Hashable)

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
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, HasTypeReps)

newtype Slot = Slot { unSlot :: Word64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, HasTypeReps)

-- | A number of slots.
--
--  We use this newtype to distinguish between a cardinal slot and a relative
--  period of slots, and also to distinguish between number of slots and number
--  of blocks.
newtype SlotCount = SlotCount { unSlotCount :: Word64 }
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num, Hashable)

instance HasTypeReps SlotCount

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

-- | An alias for 'minusSlot'
(-.) :: Slot -> SlotCount -> Slot
(-.) = minusSlot

infixl 6 -.

-- | Multiply the block count by the given constant. This function does not
-- check for overflow.
(*.) :: Word64 -> BlockCount -> SlotCount
n *. (BlockCount c) = SlotCount $ n * c

infixl 7 *.

-- | Subtract a slot count from a slot.
--
-- In case the slot count is greater than the slot's index, it returns
-- Nothing.
minusSlotMaybe :: Slot -> SlotCount -> Maybe Slot
minusSlotMaybe (Slot m) (SlotCount n)
  | m < n     = Nothing
  | otherwise = Just . Slot $ m - n

newtype BlockCount = BlockCount { unBlockCount :: Word64 }
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num, Hashable)

instance HasTypeReps BlockCount


---------------------------------------------------------------------------------
-- Transactions
---------------------------------------------------------------------------------

-- |The address of a transaction output, used to identify the owner.
newtype Addr = Addr VKey
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Hashable, HasOwner)

-- | Create an address from a number.
mkAddr :: Natural -> Addr
mkAddr = Addr . VKey . Owner

instance HasHash Addr where
  hash = Hash . H.hash

-- | A unit of value held by a UTxO.
--
newtype Lovelace = Lovelace
  { unLovelace :: Integer
  } deriving stock (Show)
    deriving newtype (Eq, Ord, Num, Hashable)
    deriving (Semigroup, Monoid) via (Sum Integer)


---------------------------------------------------------------------------------
-- Domain restriction and exclusion
---------------------------------------------------------------------------------

class Relation m where
  type Domain m :: *
  type Range m :: *

  singleton :: Domain m -> Range m -> m

  -- | Domain
  dom :: Ord (Domain m) => m -> Set (Domain m)
  -- | Range
  range :: Ord (Range m) => m -> Set (Range m)

  -- | Domain restriction
  --
  -- Unicode: 25c1
  (◁), (<|) :: (Ord (Domain m), Foldable f) => f (Domain m) -> m -> m
  s <| r = s ◁ r

  -- | Domain exclusion
  --
  -- Unicode: 22ea
  (⋪), (</|) :: (Ord (Domain m), Foldable f) => f (Domain m) -> m -> m
  s </| r = s ⋪ r

  -- | Range restriction
  --
  -- Unicode: 25b7
  (▷), (|>) :: Ord (Range m) => m -> Set (Range m) -> m
  s |> r = s ▷ r

  -- | Union
  (∪) :: (Ord (Domain m), Ord (Range m)) => m -> m -> m

  -- | Union Override
  (⨃) :: (Ord (Domain m), Ord (Range m), Foldable f) => m -> f (Domain m, Range m) -> m

  -- | Restrict range to values less or equal than the given value
  --
  -- Unicode: 25b7
  (▷<=) :: (Ord (Range m)) => m -> Range m -> m
  infixl 5 ▷<=

  -- | Restrict range to values greater or equal than the given value
  --
  -- Unicode: 25b7
  (▷>=) :: (Ord (Range m)) => m -> Range m -> m
  infixl 5 ▷>=


  -- | Size of the relation
  size :: Integral n => m -> n

-- | Alias for 'elem'.
--
-- Unicode: 2208
(∈) :: (Eq a, Foldable f) => a -> f a -> Bool
a ∈ f = elem a f

-- | Alias for not 'elem'.
--
-- Unicode: 2209
(∉) :: (Eq a, Foldable f) => a -> f a -> Bool
a ∉ f = not $ elem a f

infixl 4 ∉

instance (Ord k, Ord v) => Relation (Bimap k v) where
  type Domain (Bimap k v) = k
  type Range (Bimap k v) = v

  singleton = Bimap.singleton

  dom = Set.fromList . Bimap.keys
  range = Set.fromList . Bimap.elems

  s ◁ r = Bimap.filter (\k _ -> k `Set.member` toSet s) r

  s ⋪ r = Bimap.filter (\k _ -> k `Set.notMember` toSet s) r

  r ▷ s = Bimap.filter (\_ v -> Set.member v s) r

  d0 ∪ d1 = Bimap.fold Bimap.insert d0 d1
  d0 ⨃ d1 = foldr (uncurry Bimap.insert) d0 (toList d1)

  r ▷<= vmax = Bimap.filter (\_ v -> v <= vmax) r

  r ▷>= vmax = Bimap.filter (\_ v -> v >= vmax) r

  size = fromIntegral . Bimap.size

instance Relation (Map k v) where
  type Domain (Map k v) = k
  type Range (Map k v) = v

  singleton = Map.singleton

  dom = Map.keysSet
  range = Set.fromList . Map.elems

  s ◁ r = Map.filterWithKey (\k _ -> k `Set.member` toSet s) r

  s ⋪ r = Map.filterWithKey (\k _ -> k `Set.notMember` toSet s) r

  r ▷ s = Map.filter (flip Set.member s) r

  d0 ∪ d1 = Map.union d0 d1
  -- For union override we pass @d1@ as first argument, since 'Map.union' is
  -- left biased.
  d0 ⨃ d1 = Map.union (Map.fromList . toList $ d1) d0

  r ▷<= vmax = Map.filter (<= vmax) r

  r ▷>= vmax = Map.filter (>= vmax) r

  size = fromIntegral . Map.size

instance Relation (Set (a, b)) where
  type Domain (Set (a, b)) = a
  type Range (Set (a, b))  = b

  singleton a b = Set.singleton (a,b)

  dom = Set.map fst
  range = Set.map snd

  s ◁ r = Set.filter (\(k,_) -> k `Set.member` toSet s) r

  s ⋪ r = Set.filter (\(k,_) -> k `Set.notMember` toSet s) r

  r ▷ s = Set.filter (\(_,v) -> Set.member v s) r

  (∪) = Set.union

  d0 ⨃ d1 = d1' ∪ ((dom d1') ⋪ d0)
    where
      d1' = toSet d1

  r ▷<= vmax = Set.filter ((<= vmax) . snd) $ r

  r ▷>= vmax = Set.filter ((>= vmax) . snd) $ r

  size = fromIntegral . Set.size


---------------------------------------------------------------------------------
-- Aliases
---------------------------------------------------------------------------------

-- | Inclusion among foldables.
--
-- Unicode: 2286
--
(⊆) :: (Foldable f, Foldable g, Ord a) => f a -> g a -> Bool
x ⊆ y = toSet x `isSubsetOf` toSet y

toSet :: (Foldable f, Ord a) => f a -> Set a
toSet = Set.fromList . toList
