{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.Properties
  ( tests,
  )
where

import Cardano.Chain.Slotting
  ( SlotCount (..),
    SlotNumber (..),
    addSlotCount,
    fromSlotNumber,
    toSlotNumber,
  )
import Cardano.Prelude
import Hedgehog (forAll, property, success, (===))
import Test.Cardano.Chain.Slotting.Gen
  ( genConsistentEpochAndSlotCountEpochSlots,
    genEpochAndSlotCount,
    genEpochSlots,
    genSlotCount,
    genSlotNumber,
  )
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, withTestsTS)

--------------------------------------------------------------------------------
-- EpochAndSlotCount
--------------------------------------------------------------------------------

-- Check that `fromSlotNumber` does not panic for
-- allowed values of `EpochSlots` and `SlotNumber`.
ts_prop_fromSlotNumber :: TSProperty
ts_prop_fromSlotNumber = withTestsTS 100 . property $ do
  sc <- forAll genEpochSlots
  fsId <- forAll $ genSlotNumber
  _ <- pure $ fromSlotNumber sc fsId
  success

-- Check that `fromSlotNumber . toSlotNumber == id`.
ts_prop_unflattenFlattenEpochAndSlotCount :: TSProperty
ts_prop_unflattenFlattenEpochAndSlotCount = withTestsTS 100 . property $ do
  (sId, sc) <- forAll genConsistentEpochAndSlotCountEpochSlots
  sId === fromSlotNumber sc (toSlotNumber sc sId)

-- Check that `genEpochAndSlotCount` does not panic for
-- allowed values of `EpochSlots`.
ts_prop_genEpochAndSlotCount :: TSProperty
ts_prop_genEpochAndSlotCount = withTestsTS 100 . property $ do
  sc <- forAll genEpochSlots
  _ <- forAll $ genEpochAndSlotCount sc
  success

-- Check that `toSlotNumber . fromSlotNumber == id`.
ts_prop_fromToSlotNumber :: TSProperty
ts_prop_fromToSlotNumber = withTestsTS 100 . property $ do
  es <- forAll genEpochSlots
  slot <- forAll genSlotNumber
  let fromTo = toSlotNumber es $ fromSlotNumber es slot
  slot === fromTo

-- Check that `addSlotCount` actually adds.
ts_prop_addSlotCount :: TSProperty
ts_prop_addSlotCount = withTestsTS 100 . property $ do
  sc <- forAll genSlotCount
  fs <- forAll genSlotNumber
  let added = fs + (SlotNumber $ unSlotCount sc)
  addSlotCount sc fs
    === if unSlotNumber fs <= maxBound - (unSlotCount sc)
      then added
      else SlotNumber maxBound

tests :: TSGroup
tests = $$discoverPropArg
