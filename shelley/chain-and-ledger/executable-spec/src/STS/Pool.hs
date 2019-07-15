{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Pool
  ( POOL
  )
where

import           Lens.Micro                     ( (^.) )

import           Address
import           Delegation.Certificates
import           Keys
import           LedgerState
import           PParams
import           Slot

import           Control.State.Transition

data POOL hashAlgo dsignAlgo

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (POOL hashAlgo dsignAlgo)
 where
  type State (POOL hashAlgo dsignAlgo) = PState hashAlgo dsignAlgo
  type Signal (POOL hashAlgo dsignAlgo) = DCert hashAlgo dsignAlgo
  type Environment (POOL hashAlgo dsignAlgo) = (Slot, Ptr, PParams)
  data PredicateFailure (POOL hashAlgo dsignAlgo)
    = StakePoolNotRegisteredOnKeyPOOL
    | StakePoolRetirementWrongEpochPOOL
    | WrongCertificateTypePOOL
    deriving (Show, Eq)

  initialRules = [pure emptyPState]
  transitionRules = [poolDelegationTransition]

poolDelegationTransition
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (POOL hashAlgo dsignAlgo)
poolDelegationTransition = do
  TRC ((slot, p@(Ptr _ _ _), pp), ps, c) <- judgmentContext
  case c of
    RegPool _              -> pure $ applyDCertPState p c ps
    RetirePool _ (Epoch e) -> do
      validStakePoolRetire c ps == Valid ?! StakePoolNotRegisteredOnKeyPOOL
      let Epoch cepoch   = epochFromSlot slot
      let Epoch maxEpoch = pp ^. eMax
      cepoch < e && e < cepoch + maxEpoch ?! StakePoolRetirementWrongEpochPOOL
      pure $ applyDCertPState p c ps
    _ -> do
      failBecause WrongCertificateTypePOOL
      pure $ applyDCertPState p c ps
