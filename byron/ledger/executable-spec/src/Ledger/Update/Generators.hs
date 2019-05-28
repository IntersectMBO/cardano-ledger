{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Generators for the 'Ledger.Update' values.
module Ledger.Update.Generators
  ( pparamsGen
  , protVerGen
  -- PVBUMP environment generators
  , pvbumpAfter2kEnvGen
  , pvbumpEmptyListEnvGen
  , pvbumpBeginningsEnvGen
  -- PVBUMP state generators
  , pvbumpStateGen
  )
where

import           Control.State.Transition (Environment, State)
import           Data.Word (Word64)
import           Hedgehog
import           Hedgehog.Gen.Aux (doubleInc)
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range
import           Ledger.Core (Slot(..), SlotCount(..), BlockCount(..))
import           Ledger.Core.Generators (slotGen)
import           Ledger.GlobalParams (k)
import           Ledger.Update (ProtVer(..), PParams(..), PVBUMP)
import           Numeric.Natural (Natural)


-- | Generates a 'ProtVer'
protVerGen :: Gen ProtVer
protVerGen =
  (\a b alt -> ProtVer a b alt)
    <$> Gen.integral (Range.linear (0 :: Natural) 100)
    <*> Gen.integral (Range.linear (0 :: Natural) 100)
    <*> Gen.integral (Range.linear (0 :: Natural) 100)

-- | Generates valid protocol parameters
--
-- TODO: The protocol parameters still need to be aligned with the formal
-- spec.
pparamsGen :: Gen PParams
pparamsGen =
  (\((maxBkSz, maxHdrSz, maxTxSz, maxPropSz) :: (Natural, Natural, Natural, Natural))
    (bkSgnCntT :: Double)
    ((bkSlotsPerEpoch, upTtl, stableAfter) :: (SlotCount, SlotCount, BlockCount))
    (scriptVersion :: Natural)
    (cfmThd :: Int)
    (upAdptThd :: Int)
    (factorA :: Int)
    (factorB :: Int)
    -> PParams
      maxBkSz
      maxHdrSz
      maxTxSz
      maxPropSz
      bkSgnCntT
      bkSlotsPerEpoch
      upTtl
      scriptVersion
      cfmThd
      upAdptThd
      stableAfter
      factorA
      factorB
  )
    <$> szGen
    <*> doubleInc                                       -- bkSgnCntT
    <*> slotBlockGen
    <*> Gen.integral (Range.linear (0 :: Natural) 1000) -- scriptVersion
    <*> Gen.integral (Range.linear 0 1000)              -- cfmThd
    <*> Gen.integral (Range.linear 1 100)               -- upAdptThd
    <*> pure 0                                          -- factor @a@
    <*> pure 0                                          -- factor @b@
 where
  -- | Generates maxBkSz, maxHdrSz, maxTxSz and maxPropSz
  szGen :: Gen (Natural, Natural, Natural, Natural)
  szGen = do
    bkSize <- Gen.integral (Range.linear 1 hi)
    (bkSize,,,)
      <$> gRange bkSize
      <*> gRange bkSize
      <*> gRange bkSize
   where
    lo = 1       :: Natural
    -- In mainnet the maximum header size is set to 2000000 and the maximum
    -- block size is also set to 2000000, so we have to make sure we cover
    -- those values here. The upper bound is arbitrary though.
    hi = 4000000 :: Natural
    gRange :: Natural -> Gen Natural
    gRange upper = Gen.integral (Range.linear lo upper)

  -- | Generates bkSlotsPerEpoch, upTtl and stableAfter
  slotBlockGen :: Gen (SlotCount, SlotCount, BlockCount)
  slotBlockGen =
    -- The number of slots per epoch is computed from 'k':
    -- slots per-epoch = k * 10
    let perEpoch = SlotCount $ unBlockCount k *  10 in
    (perEpoch,,)
      <$> (SlotCount  <$> gRange perEpoch)
      <*> (BlockCount <$> gRange perEpoch)
   where
    gRange :: SlotCount -> Gen Word64
    gRange hi = Gen.word64 (Range.linear 1 (unSlotCount hi))

listGen :: Word64 -> Word64 -> Int -> Int -> Gen [(Slot, (ProtVer, PParams))]
listGen loSl hiSl loLen hiLen = Gen.list (Range.linear loLen hiLen) inOneSlot
 where
  inOneSlot :: Gen (Slot, (ProtVer, PParams))
  inOneSlot = (\s v p -> (s, (v, p)))
    <$> slotGen loSl hiSl
    <*> protVerGen
    <*> pparamsGen

-- | Generates an environment for the PVBUMP STS with an empty list of
-- updates
pvbumpEmptyListEnvGen :: Gen (Environment PVBUMP)
pvbumpEmptyListEnvGen = (, []) <$> slotGen 0 100000

-- | Generates an environment for the PVBUMP STS such that s_n <= 2 *
-- k
pvbumpBeginningsEnvGen :: Gen (Environment PVBUMP)
pvbumpBeginningsEnvGen =
  (,)
    <$> (slotGen 0 $ 2 * (unBlockCount k))
    <*> listGen 0 100000 0 10

-- | Generates an environment for the PVBUMP STS such that s_n >= 2 *
-- k
pvbumpAfter2kEnvGen :: Gen (Environment PVBUMP)
pvbumpAfter2kEnvGen =
  let kv = unBlockCount k in (,)
    <$> slotGen (2 * kv + 1) (10 * kv)
    <*> ((++)
         <$> listGen 0 1 1 1 -- to ensure there is at least one
                             -- element left after domain restriction
                             -- in the lastProposal property
         <*> listGen 0 (10 * kv) 1 10
        )

-- | Generates a state value for the PVBUMP STS
pvbumpStateGen :: Gen (State PVBUMP)
pvbumpStateGen = (,) <$> protVerGen <*> pparamsGen
