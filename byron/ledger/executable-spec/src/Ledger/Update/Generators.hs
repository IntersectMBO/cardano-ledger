{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Generators for the 'Ledger.Update' values.
module Ledger.Update.Generators
  ( pparamsGen
  , protVerGen
  , ppsUpdate
  -- * PVBUMP environment generators
  , pvbumpAfter2kEnvGen
  , pvbumpEmptyListEnvGen
  , pvbumpBeginningsEnvGen
  -- * PVBUMP state generators
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
    (upAdptThd :: Double)
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
    <*> Gen.double (Range.constant 0 1)                 -- upAdptThd
    <*> Gen.int (Range.linear 0 100)                    -- factor @a@
    <*> Gen.int (Range.linear 0 10)                     -- factor @b@
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
  slotBlockGen = do
    -- The number of slots per epoch is computed from 'k':
    -- slots per-epoch = k * 10
    k <- BlockCount <$> Gen.integral (Range.linear 1 10000)
    let perEpoch = SlotCount $ (unBlockCount k) * 10
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
pvbumpEmptyListEnvGen = (, [], )
  <$> slotGen 0 100000
  <*> (BlockCount <$> Gen.integral (Range.linear 1 10000))

-- | Generates an environment for the PVBUMP STS such that s_n <= 2 *
-- k
pvbumpBeginningsEnvGen :: Gen (Environment PVBUMP)
pvbumpBeginningsEnvGen =
  (\(k, s) l -> (s, l, k))
    <$> ksGen
    <*> listGen 0 100000 0 10
 where
  ksGen :: Gen (BlockCount, Slot)
  ksGen = do
    k <- BlockCount <$> Gen.integral (Range.linear 1 10000)
    s <- slotGen 0 $ 2 * (unBlockCount k)
    pure (k, s)

-- | Generates an environment for the PVBUMP STS such that s_n > 2 *
-- k
pvbumpAfter2kEnvGen :: Gen (Environment PVBUMP)
pvbumpAfter2kEnvGen = do
  k <- BlockCount <$> Gen.integral (Range.linear 1 10000)
  let kv = unBlockCount k
  s <- slotGen (2 * kv + 1) (10 * kv)
  (\l -> (s, l, k))
    <$> ((++)
         <$> listGen 0 1 1 1 -- to ensure there is at least one
                             -- element left after domain restriction
                             -- in the lastProposal property
         <*> listGen 0 (10 * kv) 1 10
        )

-- | Generates a state value for the PVBUMP STS
pvbumpStateGen :: Gen (State PVBUMP)
pvbumpStateGen = (,) <$> protVerGen <*> pparamsGen

-- | Generate a protocol parameter update from a given set of current
-- protocol-parameters, ensuring the consistency of the new protocol parameters
-- w.r.t. the current ones, according to the @canUpdate@ predicate in the
-- formal specification.
ppsUpdate :: PParams -> Gen PParams
ppsUpdate pps = do
  -- Determine the change in the block size: a decrement or an increment that
  -- is no more than twice the current block maximum size.
  --
  -- We don't expect the maximum block size to change often, so we generate
  -- more values around the current block size (@_maxBkSz@).
  newMaxBkSize <- Gen.integral (Range.exponentialFrom _maxBkSz 1 (2 * _maxBkSz))
                  `increasingProbabilityAt`
                  (1, 2 * _maxBkSz)

  -- Similarly, we don't expect the transaction size to be changed often, so we
  -- also generate more values around the current maximum transaction size.
  newMaxTxSize <- Gen.integral (Range.exponentialFrom _maxTxSz 0 (newMaxBkSize - 1))
                  `increasingProbabilityAt`
                  (0, newMaxBkSize - 1)

  PParams
    <$> pure newMaxBkSize
    <*> nextMaxHdrSzGen
    <*> pure newMaxTxSize
    <*> nextMaxPropSz
    <*> nextBkSgnCntT
    <*> pure _bkSlotsPerEpoch -- This parameter should be removed from 'PParams'
    <*> nextUpTtl
    <*> nextScriptVersion
    <*> nextCfmThd
    <*> nextUpAdptThd
    <*> pure _stableAfter     -- This parameter should be removed from 'PParams'
    <*> nextFactorA
    <*> nextFactorB

  where
    PParams{ _maxBkSz
           , _maxHdrSz
           , _maxTxSz
           , _maxPropSz
           , _bkSgnCntT
           , _bkSlotsPerEpoch
           , _upTtl
           , _scriptVersion
           , _cfmThd
           , _upAdptThd
           , _stableAfter
           , _factorA
           , _factorB
           } = pps

    nextMaxHdrSzGen :: Gen Natural
    nextMaxHdrSzGen =
      Gen.integral (Range.exponentialFrom _maxHdrSz 0 (2 * _maxHdrSz))
      `increasingProbabilityAt` (0, (2 * _maxHdrSz))

    nextMaxPropSz :: Gen Natural
    nextMaxPropSz =
      Gen.integral (Range.exponentialFrom _maxPropSz 0 (2 * _maxPropSz))
      `increasingProbabilityAt` (0, 2 * _maxPropSz)

    nextBkSgnCntT :: Gen Double
    nextBkSgnCntT =
      Gen.double (Range.exponentialFloatFrom _bkSgnCntT 0 1)
      `increasingProbabilityAt` (0, 1)

    nextUpTtl :: Gen SlotCount
    nextUpTtl = SlotCount <$>
      -- TODO: here we need to decide what is right the minimum value for the
      -- update-proposal TTL, and maybe adapt the rules to check the value of
      -- this parameter cannot change to anything below this value.
      --
      -- For now we choose an arbitrary constant.
      Gen.integral (Range.exponentialFrom currUpTtl minTtl (2 * currUpTtl))
      `increasingProbabilityAt` (minTtl, 2 * currUpTtl)
      where
        SlotCount currUpTtl = _upTtl
        minTtl = 2

    -- The new script version can be increased at most 1 unit
    nextScriptVersion :: Gen Natural
    nextScriptVersion = Gen.element [_scriptVersion, _scriptVersion + 1]

    nextCfmThd :: Gen Int
    nextCfmThd = Gen.integral (Range.exponentialFrom 0 _cfmThd (_cfmThd + 1))
      `increasingProbabilityAt`
      (0, _cfmThd + 1)

    nextUpAdptThd :: Gen Double
    nextUpAdptThd =
      Gen.double (Range.exponentialFloatFrom _upAdptThd 0 1)
      `increasingProbabilityAt` (0, 1)

    nextFactorA :: Gen Int
    nextFactorA =
      -- TODO: we choose arbitrary numbers here for now.
      Gen.integral (Range.exponentialFrom _factorA 0 10)
      `increasingProbabilityAt` (0, 10)

    nextFactorB :: Gen Int
    nextFactorB =
      -- TODO: we choose arbitrary numbers here for now.
      Gen.integral (Range.exponentialFrom _factorB 0 10)
      `increasingProbabilityAt` (0, 10)

-- | Generate values the given distribution in 90% of the cases, and values at
-- the bounds of the range in 10% of the cases.
--
-- This can be used to generate enough extreme values. The exponential and
-- linear distributions provided by @hedgehog@ will generate a small percentage
-- of these (0-1%).
increasingProbabilityAt
  :: Gen a
  -> (a, a)
  -> Gen a
increasingProbabilityAt gen (lower, upper)
  = Gen.frequency [ (5, pure lower)
                  , (90, gen)
                  , (5, pure upper)
                  ]
