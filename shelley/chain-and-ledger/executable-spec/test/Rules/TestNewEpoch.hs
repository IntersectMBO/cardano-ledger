{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestNewEpoch
  ( rewardDecreaseEqualsTreasuryRewardPot
  , circulationDepositsInvariant)
where

import           Test.QuickCheck (Property, conjoin)

import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     source, target)

import           Coin (pattern Coin)
import           LedgerState (pattern AccountState, pattern DPState, pattern DState,
                     pattern EpochState, pattern LedgerState, pattern NewEpochState,
                     pattern UTxOState, esAccountState, esLState, nesEs, _delegationState,
                     _deposited, _dstate, _fees, _reserves, _rewards, _treasury, _utxo, _utxoState)
import           MockTypes (NEWEPOCH)
import           UTxO (balance)

-----------------------------
-- Properties for NEWEPOCH --
-----------------------------

-- | Check that the rewards decrease by the increase of the treasury and the
-- rewards.
rewardDecreaseEqualsTreasuryRewardPot
  :: [SourceSignalTarget NEWEPOCH]
  -> Property
rewardDecreaseEqualsTreasuryRewardPot tr =
  conjoin $
    map rewardsDecreaseBalanced tr

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
circulationDepositsInvariant
  :: [SourceSignalTarget NEWEPOCH]
  -> Property
circulationDepositsInvariant tr =
  conjoin $
    map circulationDepositsNoChange tr

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
