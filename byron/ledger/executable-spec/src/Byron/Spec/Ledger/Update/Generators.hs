{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Generators for the 'Ledger.Update' values.
module Byron.Spec.Ledger.Update.Generators (pparamsGen) where

import Byron.Spec.Ledger.Core
  ( BlockCount (BlockCount),
    SlotCount (SlotCount),
    unBlockCount,
    unSlotCount,
  )
import Byron.Spec.Ledger.Update
  ( BkSgnCntT (..),
    FactorA (..),
    FactorB (..),
    PParams (PParams),
    UpAdptThd (..),
  )
import Data.Word (Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Gen.Double (doubleInc)
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

-- | Generates valid protocol parameters
--
-- TODO: The protocol parameters still need to be aligned with the formal
-- spec.
pparamsGen :: Gen PParams
pparamsGen =
  ( \((maxBkSz, maxHdrSz, maxTxSz, maxPropSz) :: (Natural, Natural, Natural, Natural))
     (bkSgnCntTDouble :: Double)
     ((bkSlotsPerEpoch, upTtl) :: (SlotCount, SlotCount))
     (scriptVersion :: Natural)
     (_cfmThd :: Double)
     (upAdptThdDouble :: Double)
     (factorAInt :: Int)
     (factorBInt :: Int) ->
        PParams
          maxBkSz
          maxHdrSz
          maxTxSz
          maxPropSz
          (BkSgnCntT bkSgnCntTDouble)
          bkSlotsPerEpoch
          upTtl
          scriptVersion
          (UpAdptThd upAdptThdDouble)
          (FactorA factorAInt)
          (FactorB factorBInt)
  )
    <$> szGen
    <*> doubleInc -- bkSgnCntT
    <*> slotBlockGen
    <*> Gen.integral (Range.linear (0 :: Natural) 1000) -- scriptVersion
    <*> Gen.double (Range.constant 0 1) -- cfmThd
    <*> Gen.double (Range.constant 0 1) -- upAdptThd
    <*> Gen.int (Range.linear 0 100) -- factor @a@
    <*> Gen.int (Range.linear 0 10) -- factor @b@
  where
    szGen :: Gen (Natural, Natural, Natural, Natural)
    szGen = do
      bkSize <- Gen.integral (Range.linear 1 hi)
      (bkSize,,,)
        <$> gRange bkSize
        <*> gRange bkSize
        <*> gRange bkSize
      where
        lo = 1 :: Natural
        -- In mainnet the maximum header size is set to 2000000 and the maximum
        -- block size is also set to 2000000, so we have to make sure we cover
        -- those values here. The upper bound is arbitrary though.
        hi = 4000000 :: Natural
        gRange :: Natural -> Gen Natural
        gRange upper = Gen.integral (Range.linear lo upper)

    slotBlockGen :: Gen (SlotCount, SlotCount)
    slotBlockGen = do
      -- The number of slots per epoch is computed from 'k':
      -- slots per-epoch = k * 10
      k <- BlockCount <$> Gen.integral (Range.linear 1 10000)
      let perEpoch = SlotCount $ unBlockCount k * 10
      (perEpoch,)
        <$> (SlotCount <$> gRange perEpoch)
      where
        gRange :: SlotCount -> Gen Word64
        gRange hi = Gen.word64 (Range.linear 1 (unSlotCount hi))
