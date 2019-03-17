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
  , EpochSlottingData(..)
  , FlatSlotId(..)
  , LocalSlotIndex
  , EpochSlots(..)
  , SlotId(..)
  , SlottingData
  , getSlotIndex
  , localSlotIndexMaxBound
  , localSlotIndexMinBound
  , mkLocalSlotIndex
  , mkSlottingData
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
  lb = getSlotIndex localSlotIndexMinBound
  ub = getSlotIndex (localSlotIndexMaxBound epochSlots)
  mkLocalSlotIndex' slot = case mkLocalSlotIndex epochSlots slot of
    Left err -> panic $ sformat
      ("The impossible happened in genLocalSlotIndex: " . build)
      err
    Right lsi -> lsi

-- Restricted to upper bound of `Word16` because `mkLocalSlotIndex`
-- creates a `LocalSlotIndex` which is limited to a `Word16`.
genEpochSlots :: Gen EpochSlots
genEpochSlots = EpochSlots <$> Gen.word64 Range.constantBounded

genSlotId :: EpochSlots -> Gen SlotId
genSlotId epochSlots =
  SlotId <$> genEpochIndex <*> genLocalSlotIndex epochSlots

-- Generates a `SlotId` and a `EpochSlots` that does not exceed
-- the `Word64` maximum boundary of `flattenSlotId` when flattened.
genConsistentSlotIdEpochSlots :: Gen (SlotId, EpochSlots)
genConsistentSlotIdEpochSlots = do
  sc  <- genLsiEpochSlots
  lsi <- genLocalSlotIndex sc
  eI  <- genRestrictedEpochIndex $ maxBound `div` getEpochSlots sc
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
  epochSlots <- EpochSlots . fromIntegral <$> Gen.word16 Range.constantBounded
  genA pm epochSlots
