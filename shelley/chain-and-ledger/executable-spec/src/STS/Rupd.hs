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

data RUPD crypto

data RupdEnv crypto
  = RupdEnv (BlocksMade crypto) (EpochState crypto)

instance STS (RUPD crypto) where
  type State (RUPD crypto) = Maybe (RewardUpdate crypto)
  type Signal (RUPD crypto) = Slot.Slot
  type Environment (RUPD crypto) = RupdEnv crypto
  data PredicateFailure (RUPD crypto)
    = FailureRUPD
    deriving (Show, Eq)

  initialRules = [pure Nothing]
  transitionRules = [rupdTransition]

rupdTransition :: TransitionRule (RUPD crypto)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  let slot = firstSlot (epochFromSlot s) +* startRewards
  if s <= slot
    then pure ru
    else case ru of
      Nothing -> pure $ Just (createRUpd b es)
      Just _  -> pure ru
