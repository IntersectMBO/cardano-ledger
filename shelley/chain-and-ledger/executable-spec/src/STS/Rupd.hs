{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Rupd
  ( RUPD
  )
where

import           BlockChain
import           EpochBoundary
import           LedgerState
import           Slot

import           Control.State.Transition

data RUPD hashAlgo dsignAlgo

instance STS (RUPD hashAlgo dsignAlgo) where
  type State (RUPD hashAlgo dsignAlgo) = Maybe (RewardUpdate hashAlgo dsignAlgo)
  type Signal (RUPD hashAlgo dsignAlgo) = Slot.Slot
  type Environment (RUPD hashAlgo dsignAlgo)
    = (BlocksMade hashAlgo dsignAlgo, EpochState hashAlgo dsignAlgo)
  data PredicateFailure (RUPD hashAlgo dsignAlgo)
    = FailureRUPD
    deriving (Show, Eq)

  initialRules = [pure Nothing]
  transitionRules = [rupdTransition]

rupdTransition :: TransitionRule (RUPD hashAlgo dsignAlgo)
rupdTransition = do
  TRC ((b, es), ru, s) <- judgmentContext
  let slot = (firstSlot $ epochFromSlot s) +* startRewards
  if s <= slot
    then pure ru
    else case ru of
      Nothing -> pure $ Just (createRUpd b es)
      Just _  -> pure $ ru
