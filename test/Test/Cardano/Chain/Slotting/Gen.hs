{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Slotting.Gen
  ( genEpochIndex
  , genEpochSlottingData
  , genSlottingDataTooFewIndicies
  , genSlottingDataInvalidIndicies
  , genFlatSlotId
  , genLocalSlotIndex
  , genLsiEpochSlots
  , genEpochSlots
  , genWithEpochSlots
  , genSlotId
  , genConsistentSlotIdEpochSlots
  , genSlottingData
  , feedPMEpochSlots
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.Map.Strict as Map
import Formatting (build, sformat)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Slotting
  ( EpochIndex(..)
  , EpochSlots(..)
  , EpochSlottingData(..)
  , FlatSlotId(..)
  , LocalSlotIndex
  , SlotId(..)
  , SlottingData
  , WithEpochSlots(WithEpochSlots)
  , localSlotIndexMaxBound
  , localSlotIndexMinBound
  , mkLocalSlotIndex
  , mkSlottingData
  , unLocalSlotIndex
  , unsafeSlottingData
  )
import Cardano.Crypto (ProtocolMagicId)

import Test.Cardano.Crypto.Gen (genProtocolMagicId)


genEpochIndex :: Gen EpochIndex
genEpochIndex = EpochIndex <$> Gen.word64 Range.constantBounded

-- Generates a `EpochSlots` based on `LocalSlotIndex`
genLsiEpochSlots :: Gen EpochSlots
genLsiEpochSlots = EpochSlots <$> Gen.word64 (Range.linear 1 w16Max)
 where
  w16Max :: Word64
  w16Max = fromIntegral (maxBound :: Word16)

genEpochSlottingData :: Gen EpochSlottingData
genEpochSlottingData =
  EpochSlottingData <$> genNominalDiffTime <*> genNominalDiffTime

genFlatSlotId :: Gen FlatSlotId
genFlatSlotId = FlatSlotId <$> Gen.word64 Range.constantBounded

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

genSlotId :: EpochSlots -> Gen SlotId
genSlotId epochSlots =
  SlotId <$> genEpochIndex <*> genLocalSlotIndex epochSlots

-- Generates a `SlotId` and a `EpochSlots` that does not exceed
-- the `Word64` maximum boundary of `flattenSlotId` when flattened.
genConsistentSlotIdEpochSlots :: Gen (SlotId, EpochSlots)
genConsistentSlotIdEpochSlots = do
  sc  <- genLsiEpochSlots
  lsi <- genLocalSlotIndex sc
  eI  <- genRestrictedEpochIndex $ maxBound `div` unEpochSlots sc
  pure (SlotId eI lsi, sc)
 where
  genRestrictedEpochIndex :: Word64 -> Gen EpochIndex
  genRestrictedEpochIndex bound =
    EpochIndex <$> Gen.word64 (Range.linear 0 bound)

genSlottingData :: Gen SlottingData
genSlottingData = mkSlottingData <$> genSlottingDataMap >>= \case
  Left err ->
    panic $ sformat ("The impossible happened in genSlottingData: " . build) err
  Right slottingData -> pure slottingData
 where
  genSlottingDataMap :: Gen (Map EpochIndex EpochSlottingData)
  genSlottingDataMap = do
    mapSize <- Gen.int $ Range.linear 2 10
    epochSlottingDatas <- Gen.list
      (Range.singleton mapSize)
      genEpochSlottingData
    pure $ Map.fromList $ zip
      [0 .. fromIntegral mapSize - 1]
      epochSlottingDatas

genSlottingDataTooFewIndicies :: Gen SlottingData
genSlottingDataTooFewIndicies = unsafeSlottingData <$> genSlottingDataMap
 where
  genSlottingDataMap :: Gen (Map EpochIndex EpochSlottingData)
  genSlottingDataMap = do
    mapSize <- Gen.int $ Range.linear 0 1
    epochSlottingDatas <- Gen.list
      (Range.singleton mapSize)
      genEpochSlottingData
    pure $ Map.fromList $ zip
      [0 .. fromIntegral mapSize - 1]
      epochSlottingDatas

genSlottingDataInvalidIndicies :: Gen SlottingData
genSlottingDataInvalidIndicies = unsafeSlottingData <$> genSlottingDataMap
 where
  genSlottingDataMap :: Gen (Map EpochIndex EpochSlottingData)
  genSlottingDataMap = do
    mapSize <- Gen.int $ Range.singleton 10
    rList   <- Gen.filter (\x -> x /= sort x) $ Gen.list
      (Range.singleton mapSize)
      (Gen.word64 (Range.linear 0 (fromIntegral mapSize - 1)))
    epochSlottingDatas <- Gen.list
      (Range.singleton mapSize)
      genEpochSlottingData
    pure $ Map.fromAscList $ zip (map EpochIndex rList) epochSlottingDatas

feedPMEpochSlots :: (ProtocolMagicId -> EpochSlots -> Gen a) -> Gen a
feedPMEpochSlots genA = do
  pm         <- genProtocolMagicId
  epochSlots <- genEpochSlots
  genA pm epochSlots
