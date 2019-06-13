{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Slotting.Gen
  ( genEpochNumber
  , genSlotNumber
  , genLocalSlotIndex
  , genEpochSlots
  , genWithEpochSlots
  , genSlotCount
  , genSlotId
  , genConsistentSlotIdEpochSlots
  , feedPMEpochSlots
  )
where

import Cardano.Prelude

import Formatting (build, sformat)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Slotting
  ( EpochNumber(..)
  , EpochSlots(..)
  , SlotNumber(..)
  , LocalSlotIndex
  , SlotCount(..)
  , SlotId(..)
  , WithEpochSlots(WithEpochSlots)
  , localSlotIndexMaxBound
  , localSlotIndexMinBound
  , mkLocalSlotIndex
  , unLocalSlotIndex
  )
import Cardano.Crypto (ProtocolMagicId)

import Test.Cardano.Crypto.Gen (genProtocolMagicId)


genEpochNumber :: Gen EpochNumber
genEpochNumber = EpochNumber <$> Gen.word64 Range.constantBounded

genSlotNumber :: Gen SlotNumber
genSlotNumber = SlotNumber <$> Gen.word64 Range.constantBounded

genLocalSlotIndex :: EpochSlots -> Gen LocalSlotIndex
genLocalSlotIndex epochSlots = mkLocalSlotIndex'
  <$> Gen.word16 (Range.constant lb ub)
 where
  lb = unLocalSlotIndex localSlotIndexMinBound
  ub = unLocalSlotIndex (localSlotIndexMaxBound epochSlots)
  mkLocalSlotIndex' slot = case mkLocalSlotIndex epochSlots slot of
    Left err -> panic $ sformat
      ("The impossible happened in genLocalSlotIndex: " . build)
      err
    Right lsi -> lsi

-- | Generator for slots-per-epoch. This will generate a positive number of
-- slots per-epoch, and it will have an upper bound of @maxBound :: Word16 =
-- 2^16@. The reason for this upper bound is that when converting a slot number
-- (which is an absolute value) to a pair of epoch and slot-count, this
-- slot-count, which represents a local index of a slot within the epoch and is
-- represented using a 'Word16', is calculated taking the reminder of dividing
-- the slot number by the number of slots-per-epoch ('EpochSlots'). So if the
-- generated epoch would be greater than @2^16@ we couldn't guarantee that the
-- local-index would fit inside its representation.
--
genEpochSlots :: Gen EpochSlots
genEpochSlots =
  EpochSlots . fromIntegral <$> Gen.word16 (Range.constant 1 maxBound)

-- | Generate a value wrapped on a 'WithEpochSlots' context, using the given
-- generator functions, and its arguments
genWithEpochSlots
  :: (ProtocolMagicId -> EpochSlots -> Gen a)
  -> ProtocolMagicId
  -> EpochSlots
  -> Gen (WithEpochSlots a)
genWithEpochSlots gen pm es = WithEpochSlots es <$> gen pm es

genSlotCount :: Gen SlotCount
genSlotCount = SlotCount <$> Gen.word64 Range.constantBounded

genSlotId :: EpochSlots -> Gen SlotId
genSlotId epochSlots =
  SlotId <$> genEpochNumber <*> genLocalSlotIndex epochSlots

-- Generates a `SlotId` and a `EpochSlots` that does not exceed
-- the `Word64` maximum boundary of `flattenSlotId` when flattened.
genConsistentSlotIdEpochSlots :: Gen (SlotId, EpochSlots)
genConsistentSlotIdEpochSlots = do
  es  <- genEpochSlots
  lsi <- genLocalSlotIndex es
  eI  <- genRestrictedEpochNumber $ maxBound `div` unEpochSlots es
  pure (SlotId eI lsi, es)
 where
  genRestrictedEpochNumber :: Word64 -> Gen EpochNumber
  genRestrictedEpochNumber bound =
    EpochNumber <$> Gen.word64 (Range.linear 0 bound)

feedPMEpochSlots :: (ProtocolMagicId -> EpochSlots -> Gen a) -> Gen a
feedPMEpochSlots genA = do
  pm         <- genProtocolMagicId
  epochSlots <- genEpochSlots
  genA pm epochSlots
