{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestNewEpoch
  ( preservationOfAda
  , circulationDepositsInvariant)
where

import           Test.QuickCheck (Property, conjoin)

import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     source, target)

import           ConcreteCryptoTypes (NEWEPOCH)
import           Shelley.Spec.Ledger.Coin (pattern Coin)
import           Shelley.Spec.Ledger.LedgerState (pattern AccountState, pattern DPState,
                     pattern DState, pattern EpochState, pattern LedgerState,
                     pattern NewEpochState, pattern UTxOState, esAccountState, esLState, nesEs,
                     _delegationState, _deposited, _dstate, _fees, _reserves, _rewards, _treasury,
                     _utxo, _utxoState)
import           Shelley.Spec.Ledger.UTxO (balance)

-----------------------------
-- Properties for NEWEPOCH --
-----------------------------

-- | Check that the rewards decrease by the increase of the treasury and the
-- rewards.
preservationOfAda
  :: [SourceSignalTarget NEWEPOCH]
  -> Property
preservationOfAda tr =
  conjoin $
    map rewardsDecreaseBalanced tr

  where sum_ = foldl (+) (Coin 0)

        rewardsDecreaseBalanced
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
                               _utxoState = UTxOState { _utxo = utxo,_fees = fees, _deposited = deposits_ }
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
                               _utxoState = UTxOState { _utxo = utxo', _fees = fees', _deposited = deposits' }
                             , _delegationState = DPState
                                                  { _dstate = DState
                                                              { _rewards = rewards' }
                                                  }
                             }
                }
              }
            }
          ) =
          reserves + treasury + sum_ rewards + balance utxo + fees + deposits_
          == reserves' + treasury' + sum_ rewards' + balance utxo' + fees' + deposits'

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
