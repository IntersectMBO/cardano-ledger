{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Pool
  ( POOL
  ) where

import           Lens.Micro              ((^.))

import           Delegation.Certificates
import           LedgerState
import           PParams
import           Slot
import           UTxO

import           Control.State.Transition

data POOL

instance STS POOL where
  type State POOL = PState
  type Signal POOL = DCert
  type Environment POOL = (Slot, Ptr, PParams)
  data PredicateFailure POOL = StakePoolNotRegisteredOnKeyPOOL
                           | StakePoolRetirementWrongEpochPOOL
                           | WrongCertificateTypePOOL
                               deriving (Show, Eq)
  initialRules = [pure emptyPState]
  transitionRules = [poolDelegationTransition]

poolDelegationTransition :: TransitionRule POOL
poolDelegationTransition = do
  TRC ((slot, p@(Ptr _ _ _), pp), ps, c) <- judgmentContext
  case c of
    RegPool _ -> pure $ applyDCertPState p c ps
    RetirePool _ (Epoch e) -> do
      validStakePoolRetire c ps == Valid ?! StakePoolNotRegisteredOnKeyPOOL
      let Epoch cepoch = epochFromSlot slot
      let Epoch maxEpoch = pp ^. eMax
      cepoch < e && e < cepoch + maxEpoch ?! StakePoolRetirementWrongEpochPOOL
      pure $ applyDCertPState p c ps
    _ -> do
      failBecause WrongCertificateTypePOOL
      pure $ applyDCertPState p c ps
