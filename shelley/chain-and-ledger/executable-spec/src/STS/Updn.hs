{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Updn
  ( UPDN
  , UpdnEnv(..)
  , UpdnState(..)
  )
where

import           BaseTypes
import           BlockChain
import           PParams
import           Slot

import           Control.State.Transition

data UPDN

data UpdnEnv = UpdnEnv Nonce PParams Bool
data UpdnState = UpdnState Nonce Nonce Nonce
  deriving (Show, Eq)

instance STS UPDN where
  type State UPDN = UpdnState
  type Signal UPDN = Slot.Slot
  type Environment UPDN = UpdnEnv
  data PredicateFailure UPDN = FailureUPDN
                               deriving (Show, Eq)
  initialRules = [pure (UpdnState (mkNonce 0) (mkNonce 0) (mkNonce 0))]
  transitionRules = [updTransition]

updTransition :: TransitionRule UPDN
updTransition = do
  TRC (UpdnEnv eta pp ne, UpdnState eta_0 eta_v eta_c, s) <- judgmentContext
  let Epoch e = epochFromSlot s
  pure $ UpdnState
    (if ne then eta_c ⭒ _extraEntropy pp else eta_0)
    (eta_v ⭒ eta)
    (if s +* slotsPrior < firstSlot (Epoch (e + 1))
      then eta_v ⭒ eta
      else eta_c
    )
