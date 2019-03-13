{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Deleg
  ( DELEG
  ) where

import           LedgerState
import           Delegation.Certificates
import           UTxO

import           Control.State.Transition

data DELEG

instance STS DELEG where
  type State DELEG = DWState
  type Signal DELEG = DCert
  type Environment DELEG = Ptr
  data PredicateFailure DELEG = StakeKeyAlreadyRegisteredDELEG
                            | StakeKeyNotRegisteredDELEG
                            | StakeDelegationImpossibleDELEG
                            | WrongCertificateTypeDELEG
                                deriving (Show, Eq)
  initialRules = [pure emptyDelegation]
  transitionRules = [delegationTransition]

delegationTransition :: TransitionRule DELEG
delegationTransition = do
  TRC (p, d, c) <- judgmentContext
  case c of
    RegKey _ -> do
      validKeyRegistration c d == Valid ?! StakeKeyAlreadyRegisteredDELEG
      pure $ applyDCert p c d
    DeRegKey _ -> do
      validKeyDeregistration c d == Valid ?! StakeKeyNotRegisteredDELEG
      pure $ applyDCert p c d
    Delegate _ -> do
      validStakeDelegation c d == Valid ?! StakeDelegationImpossibleDELEG
      pure $ applyDCert p c d
    _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure d
