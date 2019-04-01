{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Rupd
  ( RUPD
  ) where

import           BlockChain
import           EpochBoundary
import           LedgerState
import           Slot

import           Control.State.Transition

data RUPD

instance STS RUPD where
  type State RUPD = Maybe RewardUpdate
  type Signal RUPD = Slot.Slot
  type Environment RUPD = (BlocksMade, EpochState)
  data PredicateFailure RUPD = FailureRUPD
                               deriving (Show, Eq)
  initialRules = [pure Nothing]
  transitionRules = [rupdTransition]

rupdTransition :: TransitionRule RUPD
rupdTransition = do
  TRC ((b, es), ru, s) <- judgmentContext
  let slot = (firstSlot $ epochFromSlot s) +* startRewards
  if s <= slot
    then pure ru
    else case ru of
           Nothing -> pure $ Just (createRUpd b es)
           Just _  -> pure $ ru
