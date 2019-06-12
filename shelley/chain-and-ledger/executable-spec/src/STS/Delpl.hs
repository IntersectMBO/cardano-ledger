{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Delpl
  ( DELPL
  ) where

import           LedgerState
import           Delegation.Certificates
import           UTxO
import           PParams
import           Slot

import           Control.State.Transition

import           STS.Deleg
import           STS.Pool

data DELPL

instance STS DELPL where
    type State DELPL       = DPState
    type Signal DELPL      = DCert
    type Environment DELPL = (Slot, Ptr, PParams)
    data PredicateFailure DELPL = PoolFailure (PredicateFailure POOL)
                                | DelegFailure (PredicateFailure DELEG)
                   deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delplTransition      ]

delplTransition :: TransitionRule DELPL
delplTransition = do
  TRC((slotIx, ptr, pp), d, c) <- judgmentContext
  case c of
    RegPool    _   -> do
      ps <- trans @POOL  $ TRC ((slotIx, ptr, pp), _pstate d, c)
      pure $ d { _pstate = ps }
    RetirePool _ _ -> do
      ps <- trans @POOL  $ TRC ((slotIx, ptr, pp), _pstate d, c)
      pure $ d { _pstate = ps }
    RegKey _       -> do
      ds <- trans @DELEG $ TRC ((slotIx, ptr), _dstate d, c)
      pure $ d { _dstate = ds }
    DeRegKey _     -> do
      ds <- trans @DELEG $ TRC ((slotIx, ptr), _dstate d, c)
      pure $ d { _dstate = ds }
    Delegate _     -> do
      ds <- trans @DELEG $ TRC ((slotIx, ptr), _dstate d, c)
      pure $ d { _dstate = ds }

instance Embed POOL DELPL where
  wrapFailed = PoolFailure

instance Embed DELEG DELPL where
  wrapFailed = DelegFailure
