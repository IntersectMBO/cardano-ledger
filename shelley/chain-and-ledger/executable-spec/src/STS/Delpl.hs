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

import           Control.State.Transition

import           STS.Deleg
import           STS.Pool

data DELPL

instance STS DELPL where
    type State DELPL       = DWState
    type Signal DELPL      = DCert
    type Environment DELPL = (Ptr, PParams)
    data PredicateFailure DELPL = PoolFailure (PredicateFailure POOL)
                                | DelegFailure (PredicateFailure DELEG)
                   deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delplTransition      ]

delplTransition :: TransitionRule DELPL
delplTransition = do
  TRC((slotIx, pp), d, c) <- judgmentContext
  case c of
    RegPool    _   -> trans @POOL  $ TRC ((slotIx, pp), d, c)
    RetirePool _ _ -> trans @POOL  $ TRC ((slotIx, pp), d, c)
    RegKey _       -> trans @DELEG $ TRC (slotIx, d, c)
    DeRegKey _     -> trans @DELEG $ TRC (slotIx, d, c)
    Delegate _     -> trans @DELEG $ TRC (slotIx, d, c)

instance Embed POOL DELPL where
  wrapFailed = PoolFailure

instance Embed DELEG DELPL where
  wrapFailed = DelegFailure
