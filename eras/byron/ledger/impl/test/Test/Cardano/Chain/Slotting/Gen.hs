{-# LANGUAGE TupleSections #-}

module Test.Cardano.Chain.Slotting.Gen
  ( genEpochNumber,
    genSlotNumber,
    genEpochSlots,
    genWithEpochSlots,
    genSlotCount,
    genEpochAndSlotCount,
    genConsistentEpochAndSlotCountEpochSlots,
    feedPMEpochSlots,
  )
where

import Cardano.Chain.Slotting
  ( EpochAndSlotCount (..),
    EpochNumber (..),
    EpochSlots (..),
    SlotCount (..),
    SlotNumber (..),
    WithEpochSlots (WithEpochSlots),
  )
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Prelude
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Crypto.Gen (genProtocolMagicId)

genEpochNumber :: Gen EpochNumber
genEpochNumber = EpochNumber <$> Gen.word64 Range.constantBounded

genSlotNumber :: Gen SlotNumber
genSlotNumber = SlotNumber <$> Gen.word64 Range.constantBounded

-- | Generator for slots-per-epoch. This will generate a positive number of
-- slots per-epoch, and it will have an upper bound of @maxBound :: Word16 =
-- 2^16@. The reason for this upper bound is that when converting a slot number
-- (which is an absolute value) to a pair of epoch and slot-count, this
-- slot-count, which represents a local index of a slot within the epoch and is
-- represented using a 'Word16', is calculated taking the reminder of dividing
-- the slot number by the number of slots-per-epoch ('EpochSlots'). So if the
-- generated epoch would be greater than @2^16@ we couldn't guarantee that the
-- local-index would fit inside its representation.
genEpochSlots :: Gen EpochSlots
genEpochSlots =
  EpochSlots . fromIntegral <$> Gen.word16 (Range.constant 1 maxBound)

-- | Generate a value wrapped on a 'WithEpochSlots' context, using the given
-- generator functions, and its arguments
genWithEpochSlots ::
  (ProtocolMagicId -> EpochSlots -> Gen a) ->
  ProtocolMagicId ->
  EpochSlots ->
  Gen (WithEpochSlots a)
genWithEpochSlots gen pm es = WithEpochSlots es <$> gen pm es

genSlotCount :: Gen SlotCount
genSlotCount = SlotCount <$> Gen.word64 Range.constantBounded

genEpochAndSlotCount :: EpochSlots -> Gen EpochAndSlotCount
genEpochAndSlotCount epochSlots =
  EpochAndSlotCount <$> genEpochNumber <*> genEpochSlotCount epochSlots

-- | Generate a 'SlotCount' constrained by the number of 'EpochSlots'
genEpochSlotCount :: EpochSlots -> Gen SlotCount
genEpochSlotCount epochSlots =
  SlotCount <$> Gen.word64 (Range.linear 0 (unEpochSlots epochSlots - 1))

-- Generates a `EpochAndSlotCount` and a `EpochSlots` that does not exceed
-- the `Word64` maximum boundary of `flattenEpochAndSlotCount` when flattened.
genConsistentEpochAndSlotCountEpochSlots :: Gen (EpochAndSlotCount, EpochSlots)
genConsistentEpochAndSlotCountEpochSlots = do
  epochSlots <- genEpochSlots
  fmap (,epochSlots) $
    EpochAndSlotCount
      <$> genRestrictedEpochNumber (maxBound `div` unEpochSlots epochSlots)
      <*> genEpochSlotCount epochSlots
  where
    genRestrictedEpochNumber :: Word64 -> Gen EpochNumber
    genRestrictedEpochNumber bound =
      EpochNumber <$> Gen.word64 (Range.linear 0 bound)

feedPMEpochSlots :: (ProtocolMagicId -> EpochSlots -> Gen a) -> Gen a
feedPMEpochSlots genA = do
  pm <- genProtocolMagicId
  epochSlots <- genEpochSlots
  genA pm epochSlots
