{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Deleg
  ( DELEG
  )
where

import qualified Data.Map.Strict as Map

import           BlockChain (slotsPrior)
import           Delegation.Certificates
import           Keys
import           LedgerState
import           Slot
import           UTxO

import           Control.State.Transition

data DELEG hashAlgo dsignAlgo

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (DELEG hashAlgo dsignAlgo)
 where
  type State (DELEG hashAlgo dsignAlgo) = DState hashAlgo dsignAlgo
  type Signal (DELEG hashAlgo dsignAlgo) = DCert hashAlgo dsignAlgo
  type Environment (DELEG hashAlgo dsignAlgo) = (Slot, Ptr)
  data PredicateFailure (DELEG hashAlgo dsignAlgo)
    = StakeKeyAlreadyRegisteredDELEG
    | StakeKeyNotRegisteredDELEG
    | StakeDelegationImpossibleDELEG
    | WrongCertificateTypeDELEG
    | GenesisKeyNotInpMappingDELEG
    deriving (Show, Eq)

  initialRules = [pure emptyDState]
  transitionRules = [delegationTransition]

delegationTransition
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (DELEG hashAlgo dsignAlgo)
delegationTransition = do
  TRC ((_slot, p), d@(DState _ _ _ _ genMap (Dms _dms)), c) <- judgmentContext
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
    GenesisDelegate (gkey, vk) -> do
      Map.member gkey _dms ?! GenesisKeyNotInpMappingDELEG
      let s' = _slot +* slotsPrior
      pure $ d { _fdms = Map.insert (s', gkey) vk genMap}
    _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure d
