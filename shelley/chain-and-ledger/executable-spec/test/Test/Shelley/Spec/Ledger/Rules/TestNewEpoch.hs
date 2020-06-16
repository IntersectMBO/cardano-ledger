{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Rules.TestNewEpoch (preservationOfAda) where

import Cardano.Crypto.Hash (ShortHash)
import Control.State.Transition.Trace
  ( SourceSignalTarget,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Data.List (foldl')
import Shelley.Spec.Ledger.Coin (pattern Coin)
import Shelley.Spec.Ledger.LedgerState
  ( _delegationState,
    _deposited,
    _dstate,
    _fees,
    _reserves,
    _rewards,
    _treasury,
    _utxo,
    _utxoState,
    esAccountState,
    esLState,
    nesEs,
    pattern AccountState,
    pattern DPState,
    pattern DState,
    pattern EpochState,
    pattern LedgerState,
    pattern NewEpochState,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.UTxO (balance)
import Test.QuickCheck (Property, conjoin)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (NEWEPOCH)

-----------------------------
-- Properties for NEWEPOCH --
-----------------------------

-- | Check that the rewards decrease by the increase of the treasury and the
-- rewards.
preservationOfAda ::
  [SourceSignalTarget (NEWEPOCH ShortHash)] ->
  Property
preservationOfAda tr =
  conjoin $
    map rewardsDecreaseBalanced tr
  where
    sum_ = foldl' (+) (Coin 0)
    rewardsDecreaseBalanced
      ( SourceSignalTarget
          { source =
              NewEpochState
                { nesEs =
                    EpochState
                      { esAccountState =
                          AccountState
                            { _treasury = treasury,
                              _reserves = reserves
                            },
                        esLState =
                          LedgerState
                            { _utxoState = UTxOState {_utxo = utxo, _fees = fees, _deposited = deposits_},
                              _delegationState =
                                DPState
                                  { _dstate =
                                      DState
                                        { _rewards = rewards
                                        }
                                  }
                            }
                      }
                },
            target =
              NewEpochState
                { nesEs =
                    EpochState
                      { esAccountState =
                          AccountState
                            { _treasury = treasury',
                              _reserves = reserves'
                            },
                        esLState =
                          LedgerState
                            { _utxoState = UTxOState {_utxo = utxo', _fees = fees', _deposited = deposits'},
                              _delegationState =
                                DPState
                                  { _dstate =
                                      DState
                                        { _rewards = rewards'
                                        }
                                  }
                            }
                      }
                }
          }
        ) =
        reserves + treasury + sum_ rewards + balance utxo + fees + deposits_
          == reserves' + treasury' + sum_ rewards' + balance utxo' + fees' + deposits'
