{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestNewEpoch where

import           Data.Word (Word64)

import           Hedgehog (Property, forAll, property, withTests)

import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (pattern SourceSignalTarget, source,
                     sourceSignalTargets, target)

import           Coin (pattern Coin)
import           LedgerState (pattern AccountState, pattern DPState, pattern DState,
                     pattern EpochState, pattern LedgerState, pattern NewEpochState,
                     pattern UTxOState, esAccountState, esLState, nesEs, _delegationState,
                     _deposited, _dstate, _fees, _reserves, _rewards, _treasury, _utxo, _utxoState)
import           MockTypes (NEWEPOCH)
import           UTxO (balance)

import           Test.Utils (assertAll, testGlobals)


------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

-----------------------------
-- Properties for NEWEPOCH --
-----------------------------

-- | Check that the rewards decrease by the increase of the treasury and the
-- rewards.
rewardDecreaseEqualsTreasuryRewardPot :: Property
rewardDecreaseEqualsTreasuryRewardPot = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets
        $ forAll
        $ trace @NEWEPOCH testGlobals traceLen `ofLengthAtLeast` 1

  assertAll rewardsDecreaseBalanced tr

  where rewardsDecreaseBalanced
          (SourceSignalTarget
            {
              source = NewEpochState
              {
                nesEs = EpochState
                { esAccountState = AccountState
                                   {
                                     _treasury = treasury
                                   , _reserves = reserves
                                   }
                , esLState = LedgerState
                             {
                               _utxoState = UTxOState { _fees = fees }
                             , _delegationState = DPState
                                                  { _dstate = DState
                                                              { _rewards = rewards }
                                                  }
                             }
                }
              }
            , target = NewEpochState
              {
                nesEs = EpochState
                { esAccountState = AccountState
                                   {
                                     _treasury = treasury'
                                   , _reserves = reserves'
                                   }
                , esLState = LedgerState
                             {
                               _utxoState = UTxOState { _fees = fees' }
                             , _delegationState = DPState
                                                  { _dstate = DState
                                                              { _rewards = rewards' }
                                                  }
                             }
                }
              }
            }
          ) =
          (reserves  + fees  + treasury  + foldl (+) (Coin 0) rewards) ==
          (reserves' + fees' + treasury' + foldl (+) (Coin 0) rewards')


-- | Check that the circulation and deposits do not change in a NEWEPOCH
-- transition.
circulationDepositsInvariant :: Property
circulationDepositsInvariant = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets
        $ forAll
        $ trace @NEWEPOCH testGlobals traceLen `ofLengthAtLeast` 1

  assertAll circulationDepositsNoChange tr

  where circulationDepositsNoChange
          (SourceSignalTarget
            { source = NewEpochState
              { nesEs = EpochState
                { esLState = LedgerState
                  { _utxoState = UTxOState
                    { _utxo = u
                    , _deposited = d
                    }}}}
            , target = NewEpochState
              { nesEs = EpochState
                { esLState = LedgerState
                  { _utxoState = UTxOState
                    { _utxo = u'
                    , _deposited = d'
                    }}}}}
            ) =
          d == d' && (balance u) == (balance u')
