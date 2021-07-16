{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Byron.Spec.Ledger.Core where

import Cardano.Binary (FromCBOR, ToCBOR)
import Data.AbstractSize
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Data (Data, Typeable)
import Data.Foldable (toList)
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Monoid (Sum (..))
import qualified Data.Sequence as Seq
import Data.Set (Set, intersection, isSubsetOf)
import qualified Data.Set as Set
import Data.Typeable (typeOf)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Test.Goblin
  ( AddShrinks (..),
    GeneOps,
    Goblin (..),
    SeedGoblin (..),
    saveInBagOfTricks,
    tinkerRummagedOrConjureOrSave,
    (<$$>),
  )
import Test.Goblin.TH (deriveAddShrinks, deriveGoblin, deriveSeedGoblin)

-- | An encoded hash of part of the system.
--
-- 'Nothing' is used to signal to the elaborators (i.e. the algorithms that
-- translate abstract data into data concrete) that they should produce an
-- invalid concrete hash.
newtype Hash = Hash
  { unHash :: Maybe Int
  }
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, ToCBOR, NoThunks)
  deriving anyclass (HasTypeReps)

isValid :: Hash -> Bool
isValid = isJust . unHash

-- | Hash part of the ledger payload
class HasHash a where
  hash :: a -> Hash

---------------------------------------------------------------------------------
-- Signing and verification
---------------------------------------------------------------------------------

-- | Representation of the owner of key pair.
newtype Owner = Owner
  { unOwner :: Natural
  }
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, FromCBOR, ToCBOR, NoThunks)
  deriving anyclass (HasTypeReps)

class HasOwner a where
  owner :: a -> Owner

-- | Signing Key.
newtype SKey = SKey Owner
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, ToCBOR, NoThunks)
  deriving anyclass (HasTypeReps)

instance HasOwner SKey where
  owner (SKey o) = o

-- | Verification Key.
newtype VKey = VKey Owner
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, FromCBOR, ToCBOR, NoThunks)
  deriving anyclass (HasTypeReps)

instance HasHash VKey where
  hash = Hash . Just . H.hash

instance HasOwner VKey where
  owner (VKey o) = o

-- | A genesis key is a specialisation of a generic VKey.
newtype VKeyGenesis = VKeyGenesis {unVKeyGenesis :: VKey}
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, HasHash, FromCBOR, ToCBOR, NoThunks)
  deriving anyclass (HasTypeReps)

instance HasOwner VKeyGenesis where
  owner (VKeyGenesis vk) = owner vk

mkVKeyGenesis :: Natural -> VKeyGenesis
mkVKeyGenesis = VKeyGenesis . VKey . Owner

-- | Make a set of genesis keys. The genesis keys are continuously numbered from 0 to the given
-- number of genesis keys minus 1.
mkVkGenesisSet ::
  -- | Number of genesis keys
  Word8 ->
  Set VKeyGenesis
mkVkGenesisSet ngk = Set.fromAscList $ mkVKeyGenesis <$> [0 .. (fromIntegral ngk - 1)]

-- | Key Pair.
data KeyPair = KeyPair
  { sKey :: SKey,
    vKey :: VKey
  }
  deriving (Eq, Ord, Show, Generic, NoThunks)

instance HasTypeReps KeyPair

-- | Return a key pair for a given owner.
keyPair :: Owner -> KeyPair
keyPair o = KeyPair (SKey o) (VKey o)

-- | A digital signature.
data Sig a = Sig a Owner
  deriving (Show, Eq, Ord, Generic, Hashable, Typeable, Data, NoThunks)

-- | We need a custom instance here that returns only the top level type.
--   A generic instance would have recursed into type 'a' and since we use
--   'typeReps' to compute 'abstractSize', this would mean the size of
--   'Sig a' would include the size of 'a' (e.g. 'Tx'). This would create an
--   artificial coupling between the size of a type and it's "signature".
instance Typeable a => HasTypeReps (Sig a) where
  typeReps x = typeOf x Seq.<| Seq.empty

-- | Produce a digital signature
sign :: SKey -> a -> Sig a
sign (SKey k) d = Sig d k

-- | Verify a digital signature
verify :: Eq a => VKey -> a -> Sig a -> Bool
verify (VKey vk) vd (Sig sd sk) = vk == sk && vd == sd

---------------------------------------------------------------------------------
-- Slots and Epochs
---------------------------------------------------------------------------------

newtype Epoch = Epoch {unEpoch :: Word64}
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, Num, ToCBOR, NoThunks)
  deriving anyclass (HasTypeReps)

newtype Slot = Slot {unSlot :: Word64}
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, ToCBOR, NoThunks)
  deriving anyclass (HasTypeReps)

-- | A number of slots.
--
--  We use this newtype to distinguish between a cardinal slot and a relative
--  period of slots, and also to distinguish between number of slots and number
--  of blocks.
newtype SlotCount = SlotCount {unSlotCount :: Word64}
  deriving stock (Generic, Show, Data, Typeable)
  deriving newtype (Eq, Ord, Num, Hashable, ToCBOR, NoThunks)

instance HasTypeReps SlotCount

-- | Add a slot count to a slot.
addSlot :: Slot -> SlotCount -> Slot
addSlot (Slot n) (SlotCount m) = Slot $ m + n

-- | An alias for 'addSlot'
(+.) :: Slot -> SlotCount -> Slot
(+.) = addSlot

infixl 6 +.

-- | Subtract a slot count from a slot.
--
--   This is bounded below by 0.
minusSlot :: Slot -> SlotCount -> Slot
minusSlot (Slot m) (SlotCount n)
  | m <= n = Slot 0
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
  | m < n = Nothing
  | otherwise = Just . Slot $ m - n

newtype BlockCount = BlockCount {unBlockCount :: Word64}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num, Hashable, NoThunks, FromCBOR, ToCBOR)

instance HasTypeReps BlockCount

---------------------------------------------------------------------------------
-- Transactions
---------------------------------------------------------------------------------

-- | The address of a transaction output, used to identify the owner.
newtype Addr = Addr VKey
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, HasOwner, ToCBOR, NoThunks)
  deriving anyclass (HasTypeReps)

-- | Create an address from a number.
mkAddr :: Natural -> Addr
mkAddr = Addr . VKey . Owner

instance HasHash Addr where
  hash = Hash . Just . H.hash

-- | A unit of value held by a UTxO.
newtype Lovelace = Lovelace
  { unLovelace :: Integer
  }
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Num, Hashable, Enum, Real, Integral, ToCBOR, NoThunks)
  deriving (Semigroup, Monoid) via (Sum Integer)
  deriving anyclass (HasTypeReps)

-- | Constant amount of Lovelace in the system.
lovelaceCap :: Lovelace
lovelaceCap = Lovelace $ 45 * fromIntegral ((10 :: Int64) ^ (15 :: Int64))

---------------------------------------------------------------------------------
-- Domain restriction and exclusion
---------------------------------------------------------------------------------

class Relation m where
  type Domain m :: Type
  type Range m :: Type

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

  -- | Range exclusion
  --
  -- Unicode: 22eb
  (⋫), (|/>) :: Ord (Range m) => m -> Set (Range m) -> m
  s |/> r = s ⋫ r

  -- | Union
  (∪) :: (Ord (Domain m), Ord (Range m)) => m -> m -> m

  -- | Union Override Right
  (⨃) :: (Ord (Domain m), Ord (Range m), Foldable f) => m -> f (Domain m, Range m) -> m

  -- | Restrict domain to values less or equal than the given value.
  --
  -- Unicode: 25c1
  (<=◁) :: Ord (Domain m) => Domain m -> m -> m

  infixl 5 <=◁

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

  r ⋫ s = Bimap.filter (\_ v -> Set.notMember v s) r

  d0 ∪ d1 = Bimap.fold Bimap.insert d0 d1
  d0 ⨃ d1 = foldr (uncurry Bimap.insert) d0 (toList d1)

  vmax <=◁ r = Bimap.filter (\v _ -> v <= vmax) r

  r ▷<= vmax = Bimap.filter (\_ v -> v <= vmax) r

  r ▷>= vmin = Bimap.filter (\_ v -> v >= vmin) r

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

  r ⋫ s = Map.filter (flip Set.notMember s) r

  d0 ∪ d1 = Map.union d0 d1

  -- For union override we pass @d1@ as first argument, since 'Map.union' is
  -- left biased.
  d0 ⨃ d1 = Map.union (Map.fromList . toList $ d1) d0

  vmax <=◁ r = Map.filterWithKey (\k _ -> k <= vmax) r

  r ▷<= vmax = Map.filter (<= vmax) r

  r ▷>= vmin = Map.filter (>= vmin) r

  size = fromIntegral . Map.size

-- | Union override plus is (A\B)∪(B\A)∪{k|->v1+v2 | k|->v1 : A /\ k|->v2 : B}
(∪+) :: (Ord a, Ord b, Num b) => Map a b -> Map a b -> Map a b
a ∪+ b = ((dom a) ⋪ b) ∪ ((dom b) ⋪ a) ∪ (Map.unionWith (+) a b)

instance Relation (Set (a, b)) where
  type Domain (Set (a, b)) = a
  type Range (Set (a, b)) = b

  singleton a b = Set.singleton (a, b)

  dom = Set.map fst

  range = Set.map snd

  s ◁ r = Set.filter (\(k, _) -> k `Set.member` toSet s) r

  s ⋪ r = Set.filter (\(k, _) -> k `Set.notMember` toSet s) r

  r ▷ s = Set.filter (\(_, v) -> Set.member v s) r

  r ⋫ s = Set.filter (\(_, v) -> Set.notMember v s) r

  (∪) = Set.union

  d0 ⨃ d1 = d1' ∪ ((dom d1') ⋪ d0)
    where
      d1' = toSet d1

  vmax <=◁ r = Set.filter ((<= vmax) . fst) $ r

  r ▷<= vmax = Set.filter ((<= vmax) . snd) $ r

  r ▷>= vmax = Set.filter ((>= vmax) . snd) $ r

  size = fromIntegral . Set.size

instance Relation [(a, b)] where
  type Domain [(a, b)] = a
  type Range [(a, b)] = b

  singleton a b = [(a, b)]

  dom = toSet . fmap fst

  range = toSet . fmap snd

  s ◁ r = filter ((`Set.member` toSet s) . fst) r

  s ⋪ r = filter ((`Set.notMember` toSet s) . fst) r

  r ▷ s = filter ((`Set.member` toSet s) . snd) r

  r ⋫ s = filter ((`Set.notMember` toSet s) . snd) r

  (∪) = (++)

  -- In principle a list of pairs allows for duplicated keys.
  d0 ⨃ d1 = d0 ++ toList d1

  vmax <=◁ r = filter ((<= vmax) . fst) r

  r ▷<= vmax = filter ((<= vmax) . snd) r

  r ▷>= vmin = filter ((vmin <=) . snd) r

  size = fromIntegral . length

---------------------------------------------------------------------------------
-- Aliases
---------------------------------------------------------------------------------

-- | Inclusion among foldables.
--
-- Unicode: 2286
(⊆) :: (Foldable f, Foldable g, Ord a) => f a -> g a -> Bool
x ⊆ y = toSet x `isSubsetOf` toSet y

toSet :: (Foldable f, Ord a) => f a -> Set a
toSet = Set.fromList . toList

(∩) :: Ord a => Set a -> Set a -> Set a
(∩) = intersection

--------------------------------------------------------------------------------
-- Goblins instances
--------------------------------------------------------------------------------

deriveGoblin ''Addr
deriveGoblin ''BlockCount
deriveGoblin ''Epoch
deriveGoblin ''Owner
deriveGoblin ''Sig
deriveGoblin ''Slot
deriveGoblin ''SlotCount
deriveGoblin ''VKey
deriveGoblin ''VKeyGenesis

instance GeneOps g => Goblin g Hash where
  tinker gen =
    tinkerRummagedOrConjureOrSave
      ( (Hash . Just . (`mod` 30))
          <$$> tinker (unwrapValue <$> gen)
      )
    where
      unwrapValue (Hash (Just x)) = x
      unwrapValue (Hash Nothing) =
        error $
          "tinker Hash instance: trying to tinker with an invalid hash"
            ++ " (which contains nothing)"

  conjure = saveInBagOfTricks =<< (Hash . Just . (`mod` 30) <$> conjure)

instance GeneOps g => Goblin g Lovelace where
  tinker gen =
    tinkerRummagedOrConjureOrSave
      ( (\x -> (Lovelace x) `mod` lovelaceCap)
          <$$> tinker ((\(Lovelace x) -> x) <$> gen)
      )
  conjure = saveInBagOfTricks =<< ((`mod` lovelaceCap) . Lovelace <$> conjure)

--------------------------------------------------------------------------------
-- AddShrinks instances
--------------------------------------------------------------------------------

deriveAddShrinks ''Addr
deriveAddShrinks ''BlockCount
deriveAddShrinks ''Epoch
deriveAddShrinks ''Hash
deriveAddShrinks ''Lovelace
deriveAddShrinks ''Owner
deriveAddShrinks ''Sig
deriveAddShrinks ''Slot
deriveAddShrinks ''SlotCount
deriveAddShrinks ''VKey
deriveAddShrinks ''VKeyGenesis

--------------------------------------------------------------------------------
-- SeedGoblin instances
--------------------------------------------------------------------------------

deriveSeedGoblin ''Addr
deriveSeedGoblin ''BlockCount
deriveSeedGoblin ''Epoch
deriveSeedGoblin ''Hash
deriveSeedGoblin ''Lovelace
deriveSeedGoblin ''Owner
deriveSeedGoblin ''Slot
deriveSeedGoblin ''SlotCount
deriveSeedGoblin ''VKey
deriveSeedGoblin ''VKeyGenesis
