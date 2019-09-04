{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Updn
  ( UPDN
  , UpdnState(..)
  )
where

import           BaseTypes
import           BlockChain
import           Slot

import           Control.State.Transition

data UPDN

data UpdnState = UpdnState Seed Seed
  deriving (Show, Eq)

instance STS UPDN where
  type State UPDN = UpdnState
  type Signal UPDN = Slot.Slot
  type Environment UPDN = Seed
  data PredicateFailure UPDN = FailureUPDN
                               deriving (Show, Eq)
  initialRules = [pure (UpdnState (mkNonce 0) (mkNonce 0))]
  transitionRules = [updTransition]

updTransition :: TransitionRule UPDN
updTransition = do
  TRC (eta, UpdnState eta_v eta_c, s) <- judgmentContext
  let Epoch e = epochFromSlot s
  if s +* slotsPrior < firstSlot (Epoch (e + 1))
    then pure $ UpdnState (eta_v ⭒ eta) (eta_v ⭒ eta)
    else pure $ UpdnState (eta_v ⭒ eta) eta_c
