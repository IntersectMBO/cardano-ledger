-- |

module Ledger.Update.Generators where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Ledger.Core (SlotCount (SlotCount), unBlockCount)
import Ledger.GlobalParams (k)
import Ledger.Update (PParams (PParams))

-- | Protocol parameters generator.
--
-- TODO: The protocol parameters still need to be aligned with the formal
-- spec.
pps :: Gen PParams
pps
  = PParams
  <$> gMaxHeaderSize
  <*> gMaxBlockSize
  <*> gMaxTxSize
  <*> gMaxProposalSize
  <*> gBlockSigCntThreshold
  <*> gSlotsPerEpoch
  <*> gUpdateProposalTTL
  <*> pure 1 -- _scriptVersion
  <*> gConfirmationTreshold
  <*> gAdoptinThreshold
  <*> pure k -- _stableAfter
  <*> pure 0 -- factor @a@
  <*> pure 0 -- factor @b@
  where
    -- In mainnet the maximum header size is set to 2000000 and the maximum
    -- block size is also set to 2000000, so we have to make sure we cover
    -- those values here. The upper bound is arbitrary though.
    --
    -- A constant distribution between [@min@, @max@] will almost never
    -- generate @min@. Since we want to test the minimum size we use the
    -- 'choice' combinator.
    --
    -- TODO: For a more realistic lower bound we should figure out the
    -- minimum block size.
    gMaxHeaderSize = Gen.choice
                     [ pure 0
                     , Gen.integral (Range.constant 1 4000000)
                     ]
    gMaxBlockSize  = Gen.choice
                     [ pure 0
                     , Gen.integral (Range.constant 1 4000000)
                     ]

    gMaxTxSize = Gen.choice
                 [ pure 0
                 , Gen.integral (Range.constant 1 4000000)
                 ]
    gMaxProposalSize = Gen.choice
                       [ pure 0
                       , Gen.integral (Range.constant 1 4000000)
                       ]

    gBlockSigCntThreshold = pure (1/5) -- TODO: this needs to be aligned with the formal specs.

    -- The number of slots per epoch is computed from 'k':
    -- slots per-epoch = k * 10
    gSlotsPerEpoch = pure $! SlotCount $ unBlockCount k *  10

    gUpdateProposalTTL = SlotCount <$> Gen.integral (Range.linear 1 100)

    -- Confirmation threshold
    gConfirmationTreshold = Gen.integral (Range.linear 1 7)

    -- Update adoption threshold
    gAdoptinThreshold = Gen.integral (Range.linear 1 7)
