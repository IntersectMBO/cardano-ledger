{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Deleg
  ( DELEG
  ) where

import           LedgerState
import           Delegation.Certificates
import           UTxO
import           Slot

import           Control.State.Transition

data DELEG

instance STS DELEG where
  type State DELEG = DState
  type Signal DELEG = DCert
  type Environment DELEG = (Slot, Ptr)
  data PredicateFailure DELEG = StakeKeyAlreadyRegisteredDELEG
                            | StakeKeyNotRegisteredDELEG
                            | StakeDelegationImpossibleDELEG
                            | WrongCertificateTypeDELEG
                                deriving (Show, Eq)

  initialRules = [pure emptyDState]
  transitionRules = [delegationTransition]

delegationTransition :: TransitionRule DELEG
delegationTransition = do
  TRC ((_, p), d, c) <- judgmentContext
  case c of
    RegKey _ -> do
      validKeyRegistration c d == Valid ?! StakeKeyAlreadyRegisteredDELEG
      pure $ applyDCertDState p c d
    DeRegKey _ -> do
      validKeyDeregistration c d == Valid ?! StakeKeyNotRegisteredDELEG
      pure $ applyDCertDState p c d
    Delegate _ -> do
      validStakeDelegation c d == Valid ?! StakeDelegationImpossibleDELEG
      pure $ applyDCertDState p c d
    _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure d
