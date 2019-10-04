{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Rupd
  ( RUPD
  , RupdEnv(..)
  )
where

import           BlockChain
import           EpochBoundary
import           LedgerState
import           Slot

import           Control.State.Transition

data RUPD hashAlgo dsignAlgo vrfAlgo

data RupdEnv hashAlgo dsignAlgo vrfAlgo
  = RupdEnv (BlocksMade hashAlgo dsignAlgo) (EpochState hashAlgo dsignAlgo vrfAlgo)

instance STS (RUPD hashAlgo dsignAlgo vrfAlgo) where
  type State (RUPD hashAlgo dsignAlgo vrfAlgo) = Maybe (RewardUpdate hashAlgo dsignAlgo)
  type Signal (RUPD hashAlgo dsignAlgo vrfAlgo) = Slot.Slot
  type Environment (RUPD hashAlgo dsignAlgo vrfAlgo) = RupdEnv hashAlgo dsignAlgo vrfAlgo
  data PredicateFailure (RUPD hashAlgo dsignAlgo vrfAlgo)
    = FailureRUPD
    deriving (Show, Eq)

  initialRules = [pure Nothing]
  transitionRules = [rupdTransition]

rupdTransition :: TransitionRule (RUPD hashAlgo dsignAlgo vrfAlgo)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  let slot = firstSlot (epochFromSlot s) +* startRewards
  if s <= slot
    then pure ru
    else case ru of
      Nothing -> pure $ Just (createRUpd b es)
      Just _  -> pure ru
