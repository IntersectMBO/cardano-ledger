{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Updn
  ( UPDN
  )
where

import           BaseTypes
import           BlockChain
import           Slot

import           Control.State.Transition

data UPDN

instance STS UPDN where
  type State UPDN = (Seed, Seed)
  type Signal UPDN = Slot.Slot
  type Environment UPDN = Seed
  data PredicateFailure UPDN = FailureUPDN
                               deriving (Show, Eq)
  initialRules = [pure (mkNonce 0, mkNonce 0)]
  transitionRules = [updTransition]

updTransition :: TransitionRule UPDN
updTransition = do
  TRC (eta, (eta_v, eta_c), s) <- judgmentContext
  let Epoch e = epochFromSlot s
  if s +* slotsPrior < firstSlot (Epoch (e + 1))
    then pure (eta_v ⭒ eta, eta_v ⭒ eta)
    else pure (eta_v ⭒ eta, eta_c)
