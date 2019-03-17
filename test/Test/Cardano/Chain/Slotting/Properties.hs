{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.Properties
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Formatting (build, sformat)

import Hedgehog
  ( Property
  , withTests
  , property
  , forAll
  , (===)
  , success
  , checkSequential
  , discover
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Range as Range

import Cardano.Chain.Slotting
  ( addSlotNumber
  , FlatSlotId(..)
  , EpochSlots(..)
  , LocalSlotIndex(..)
  , flattenSlotId
  , unflattenSlotId
  , mkSlottingData
  , localSlotIndexSucc
  , localSlotIndexPred
  , getSlottingDataMap
  , localSlotIndexToEnum
  , localSlotIndexFromEnum
  , validateSlottingDataMap
  , slotIdToEnum
  , slotIdFromEnum
  , subSlotNumber
  )
import Test.Cardano.Chain.Slotting.Gen
  ( genFlatSlotId
  , genLocalSlotIndex
  , genLsiEpochSlots
  , genSlotId
  , genConsistentSlotIdEpochSlots
  , genSlottingData
  , genSlottingDataInvalidIndicies
  , genSlottingDataTooFewIndicies
  )

-- NB: `genLsiEpochSlots` is used because `LocalSlotIndex` is restricted to
-- `Word16` and therefore the highest `EpochSlots` you can have currently
-- is `maxBound :: Word16`.

--------------------------------------------------------------------------------
-- SlottingData
--------------------------------------------------------------------------------

-- Check that `mkSlottingData` does not fail for
-- allowed values of `SlottingData`.
prop_mkSlottingData :: Property
prop_mkSlottingData = eachOf
  100
  (getSlottingDataMap <$> genSlottingData)
  (assertEitherIsRight mkSlottingData)

-- Check that `mkSlottingData` fails for
-- `SlottingData` maps with too few indicies
-- i.e less than to 2.
prop_mkSlottingDataTooFewIndices :: Property
prop_mkSlottingDataTooFewIndices = eachOf
  10
  (getSlottingDataMap <$> genSlottingDataTooFewIndicies)
  (assertEitherIsLeft validateSlottingDataMap)

-- Check that `mkSlottingData` fails for
-- `SlottingData` maps with invalid indicies
-- i.e indicies are not ascending order.
prop_mkSlottingDataInvalidIndices :: Property
prop_mkSlottingDataInvalidIndices = eachOf
  100
  (getSlottingDataMap <$> genSlottingDataInvalidIndicies)
  (assertEitherIsLeft validateSlottingDataMap)

--------------------------------------------------------------------------------
-- LocalSlotIndex
--------------------------------------------------------------------------------

-- Check that `genLocalSlotIndex` does not `panic` for
-- different values of `EpochSlots`. NB: we use `genLsiEpochSlots`
-- to generate values of `EpochSlots` restricted to a maximum boundary
-- of `Word16` because `LocalSlotIndex` is restricted to `Word16` values.
prop_genLocalSlotIndex :: Property
prop_genLocalSlotIndex = withTests 100 . property $ do
  sc  <- forAll genLsiEpochSlots
  lsi <- forAll $ genLocalSlotIndex sc
  case lsi of
    UnsafeLocalSlotIndex _ -> success

-- Check that `localSlotIndexToEnum` fails for
-- `LocalSlotIndex` values that exceed `EpochSlots`
prop_localSlotIndexToEnumOverflow :: Property
prop_localSlotIndexToEnumOverflow = withTests 100 . property $ do
  sc <- forAll genLsiEpochSlots
  let lsi = 1 + getEpochSlots sc
  assertEitherIsLeft (localSlotIndexToEnum sc) (fromIntegral lsi)

-- Check that `localSlotIndexToEnum` fails for
-- `LocalSlotIndex` values that are negative.
prop_localSlotIndexToEnumUnderflow :: Property
prop_localSlotIndexToEnumUnderflow = withTests 100 . property $ do
  tVal <- forAll (Gen.int (Range.constant (negate 1) minBound))
  sc   <- forAll genLsiEpochSlots
  assertEitherIsLeft (localSlotIndexToEnum sc) tVal

-- Check that `localSlotIndexPred` does not fail
-- for allowed values of `LocalSlotIndex` and `EpochSlots`.
prop_localSlotIndexPred :: Property
prop_localSlotIndexPred =
  withTests 100
    . property
    $ do
        sc  <- forAll $ Gen.filter (/= 1) genLsiEpochSlots
        -- Filter out LocalSlotIndex = 0 and EpochSlots = 1
        -- because you can't find the predecessor of the 0th slot.
        lsi <- forAll
          $ Gen.filter (/= UnsafeLocalSlotIndex 0) (genLocalSlotIndex sc)
        assertEitherIsRight (localSlotIndexPred sc) lsi

-- Check that `localSlotIndexPred` fails for
-- the lower boundary of `LocalSlotIndex`. In
-- other words, the 0th slot does not have
-- a predecessor.
prop_localSlotIndexPredMinbound :: Property
prop_localSlotIndexPredMinbound = eachOf
  100
  genLsiEpochSlots
  (assertEitherIsLeft $ flip localSlotIndexPred (UnsafeLocalSlotIndex 0))

-- Check that `localSlotIndexSucc` does not fail
-- for allowed values of `LocalSlotIndex` and `EpochSlots`.
prop_localSlotIndexSucc :: Property
prop_localSlotIndexSucc =
  withTests 100
    . property
    $ do
        sc  <- forAll genLsiEpochSlots
        -- Generate a `LocalSlotIndex` at least two less than the `EpochSlots`
        -- to avoid overflow errors as `LocalSlotIndex` starts
        -- from 0th slot.
        lsi <- forAll $ genLocalSlotIndex sc
        assertEitherIsRight (localSlotIndexSucc (sc + 2)) lsi

-- Check that `localSlotIndexSucc` fails for
-- the upper boundary of `LocalSlotIndex`. In
-- other words, the final slot does not have
-- a successor (in terms of `LocalSlotIndex`,
-- this would actually mean moving to the next epoch).
prop_localSlotIndexSuccMaxbound :: Property
prop_localSlotIndexSuccMaxbound = withTests 100 . property $ do
  sc <- forAll genLsiEpochSlots
  assertEitherIsLeft
    (localSlotIndexSucc sc)
    (UnsafeLocalSlotIndex $ 1 + (fromIntegral $ getEpochSlots sc))

-- Check that `localSlotIndexSucc . localSlotIndexPred == id`.
prop_localSlotIndexSuccPredisId :: Property
prop_localSlotIndexSuccPredisId = withTests 100 . property $ do
  sc  <- forAll genLsiEpochSlots
  lsi <- forAll $ Gen.filter (\x -> getSlotIndex x /= 0) (genLocalSlotIndex sc)
  let predSucc = localSlotIndexPred sc lsi >>= localSlotIndexSucc sc
  compareValueRight lsi predSucc

-- Check that `localSlotIndexPred . localSlotIndexSucc == id`.
prop_localSlotIndexPredSuccisId :: Property
prop_localSlotIndexPredSuccisId = withTests 100 . property $ do
  sc  <- forAll genLsiEpochSlots
  lsi <- forAll $ genLocalSlotIndex sc
  let
    succPred = localSlotIndexSucc (sc + 2) lsi >>= localSlotIndexPred (sc + 2)
  compareValueRight lsi succPred

-- Check that `localSlotIndexToEnum . localSlotIndexFromEnum == id`.
prop_localSlotIndexToEnumFromEnum :: Property
prop_localSlotIndexToEnumFromEnum = withTests 100 . property $ do
  sc   <- forAll genLsiEpochSlots
  iLsi <- forAll $ genLocalSlotIndex sc
  let fLsi = localSlotIndexToEnum sc $ localSlotIndexFromEnum iLsi
  compareValueRight iLsi fLsi

-- Check that `localSlotIndexFromEnum . localSlotIndexToEnum == id`.
prop_localSlotIndexFromEnumToEnum :: Property
prop_localSlotIndexFromEnumToEnum = withTests 100 . property $ do
  sc <- forAll genLsiEpochSlots
  let sIndex = fromIntegral $ getEpochSlots sc - 1 :: Int
  let lsi    = localSlotIndexToEnum sc sIndex
  case lsi of
    Left  err  -> failWith Nothing (show $ sformat build err)
    Right lsi' -> localSlotIndexFromEnum lsi' === sIndex

--------------------------------------------------------------------------------
-- SlotId
--------------------------------------------------------------------------------

-- Check that `slotIdFromEnum . slotIdToEnum == id`.
prop_slotIdFromEnumToEnum :: Property
prop_slotIdFromEnumToEnum = withTests 100 . property $ do
  sc <- forAll genLsiEpochSlots
  i  <- forAll $ Gen.word64 (Range.linear 0 maxBound)
  let toFrom = slotIdFromEnum sc $ slotIdToEnum sc (FlatSlotId i)
  compareValueRight (fromIntegral i) toFrom

-- Check that `slotIdToEnum . slotIdFromEnum == id`.
prop_slotIdToEnumFromEnum :: Property
prop_slotIdToEnumFromEnum = withTests 100 . property $ do
  (sId, sc) <- forAll genConsistentSlotIdEpochSlots
  case slotIdFromEnum sc sId of
    Left  err -> failWith Nothing (show $ sformat build err)
    Right int -> sId === slotIdToEnum sc (FlatSlotId $ fromIntegral int)

-- Check that `flattenSlotId` does not fail for
-- allowed values of `EpochSlots` and `SlotId`.
prop_flattenSlotId :: Property
prop_flattenSlotId = withTests 100 . property $ do
  sc  <- forAll genLsiEpochSlots
  sId <- forAll $ genSlotId sc
  assertEitherIsRight (flattenSlotId sc) sId

-- Check that `unflattenSlotId` does not panic for
-- allowed values of `EpochSlots` and `FlatSlotId`.
prop_unflattenSlotId :: Property
prop_unflattenSlotId = withTests 100 . property $ do
  sc   <- forAll genLsiEpochSlots
  fsId <- forAll $ genFlatSlotId
  _    <- pure $ unflattenSlotId sc fsId
  success

-- Check that `unflattenSlotId . flattenSlotId == id`.
prop_unflattenFlattenSlotId :: Property
prop_unflattenFlattenSlotId = withTests 100 . property $ do
  (sId, sc) <- forAll genConsistentSlotIdEpochSlots
  case flattenSlotId sc sId of
    Left  err  -> failWith Nothing (show $ sformat build err)
    Right fSid -> sId === unflattenSlotId sc fSid

-- Check that `genSlotId` does not panic for
-- allowed values of `EpochSlots`.
prop_genSlotId :: Property
prop_genSlotId = withTests 100 . property $ do
  sc <- forAll genLsiEpochSlots
  _  <- forAll $ genSlotId sc
  success

-- Check that `flattenSlotId . unflattenSlotId == id`.
prop_flattenUnflattenSlotId :: Property
prop_flattenUnflattenSlotId = withTests 100 . property $ do
  sc   <- forAll genLsiEpochSlots
  fsId <- forAll genFlatSlotId
  let unflatFlat = flattenSlotId sc $ unflattenSlotId sc fsId
  compareValueRight fsId unflatFlat

-- Check that `addSlotNumber` actually adds.
prop_addSlotNumber :: Property
prop_addSlotNumber = withTests 100 . property $ do
  sc <- forAll genLsiEpochSlots
  fs <- forAll genFlatSlotId
  let added = fs + (FlatSlotId $ getEpochSlots sc)
  addSlotNumber sc fs === added

-- Check that `addSlotNumber` actually subtracts.
prop_subSlotNumber :: Property
prop_subSlotNumber = withTests 100 . property $ do
  sc <- forAll genLsiEpochSlots
  fs <- forAll genFlatSlotId
  let subtracted = fs - (FlatSlotId $ getEpochSlots sc)
  (subSlotNumber sc fs) === subtracted

tests :: IO Bool
tests = checkSequential $$(discover)
