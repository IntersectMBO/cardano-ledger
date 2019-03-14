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
  type State POOL = DWState
  type Signal POOL = DCert
  type Environment POOL = (Ptr, PParams)
  data PredicateFailure POOL = StakePoolNotRegisteredOnKeyPOOL
                           | StakePoolRetirementWrongEpochPOOL
                           | WrongCertificateTypePOOL
                               deriving (Show, Eq)
  initialRules = [pure emptyDelegation]
  transitionRules = [poolDelegationTransition]

poolDelegationTransition :: TransitionRule POOL
poolDelegationTransition = do
  TRC ((p@(Ptr slot _ _), pp), d, c) <- judgmentContext
  case c of
    RegPool _ -> pure $ applyDCert p c d
    RetirePool _ (Epoch e) -> do
      validStakePoolRetire c d == Valid ?! StakePoolNotRegisteredOnKeyPOOL
      let Epoch cepoch = epochFromSlot slot
      let Epoch maxEpoch = pp ^. eMax
      cepoch < e && e < cepoch + maxEpoch ?! StakePoolRetirementWrongEpochPOOL
      pure $ applyDCert p c d
    _ -> do
      failBecause WrongCertificateTypePOOL
      pure $ applyDCert p c d
