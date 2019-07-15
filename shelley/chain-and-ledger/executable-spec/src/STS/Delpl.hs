{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Delpl
  ( DELPL
  )
where

import           Address
import           Keys
import           LedgerState
import           Delegation.Certificates
import           PParams hiding (d)
import           Slot
import           Tx

import           Control.State.Transition

import           STS.Deleg
import           STS.Pool

data DELPL hashAlgo dsignAlgo

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (DELPL hashAlgo dsignAlgo)
 where
  type State (DELPL hashAlgo dsignAlgo)       = DPState hashAlgo dsignAlgo
  type Signal (DELPL hashAlgo dsignAlgo)      = DCert hashAlgo dsignAlgo
  type Environment (DELPL hashAlgo dsignAlgo) =
    (Slot, Ptr, PParams, Tx hashAlgo dsignAlgo)
  data PredicateFailure (DELPL hashAlgo dsignAlgo)
    = PoolFailure (PredicateFailure (POOL hashAlgo dsignAlgo))
    | DelegFailure (PredicateFailure (DELEG hashAlgo dsignAlgo))
    deriving (Show, Eq)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delplTransition      ]

delplTransition
  :: forall hashAlgo dsignAlgo
   . (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (DELPL hashAlgo dsignAlgo)
delplTransition = do
  TRC ((slotIx, ptr, pp, _), d, c) <- judgmentContext
  case c of
    RegPool _ -> do
      ps <-
        trans @(POOL hashAlgo dsignAlgo) $ TRC ((slotIx, ptr, pp), _pstate d, c)
      pure $ d { _pstate = ps }
    RetirePool _ _ -> do
      ps <-
        trans @(POOL hashAlgo dsignAlgo) $ TRC ((slotIx, ptr, pp), _pstate d, c)
      pure $ d { _pstate = ps }
    RegKey _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo) $ TRC ((slotIx, ptr), _dstate d, c)
      pure $ d { _dstate = ds }
    DeRegKey _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo) $ TRC ((slotIx, ptr), _dstate d, c)
      pure $ d { _dstate = ds }
    Delegate _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo) $ TRC ((slotIx, ptr), _dstate d, c)
      pure $ d { _dstate = ds }
    GenesisDelegate _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo) $ TRC ((slotIx, ptr), _dstate d, c)
      pure $ d { _dstate = ds }

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Embed (POOL hashAlgo dsignAlgo) (DELPL hashAlgo dsignAlgo)
 where
  wrapFailed = PoolFailure

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Embed (DELEG hashAlgo dsignAlgo) (DELPL hashAlgo dsignAlgo)
 where
  wrapFailed = DelegFailure
