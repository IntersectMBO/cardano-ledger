{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Delpl
  ( DELPL
  , DelplEnv (..)
  )
where

import           Delegation.Certificates
import           Keys
import           LedgerState
import           PParams hiding (d)
import           Slot
import           TxData

import           Control.State.Transition

import           STS.Deleg
import           STS.Pool

data DELPL hashAlgo dsignAlgo vrfAlgo

data DelplEnv
  = DelplEnv Slot Ptr PParams

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (DELPL hashAlgo dsignAlgo vrfAlgo)
 where
  type State (DELPL hashAlgo dsignAlgo vrfAlgo)       = DPState hashAlgo dsignAlgo vrfAlgo
  type Signal (DELPL hashAlgo dsignAlgo vrfAlgo)      = DCert hashAlgo dsignAlgo vrfAlgo
  type Environment (DELPL hashAlgo dsignAlgo vrfAlgo) = DelplEnv
  data PredicateFailure (DELPL hashAlgo dsignAlgo vrfAlgo)
    = PoolFailure (PredicateFailure (POOL hashAlgo dsignAlgo vrfAlgo))
    | DelegFailure (PredicateFailure (DELEG hashAlgo dsignAlgo vrfAlgo))
    | ScriptNotInWitnessDELPL
    | ScriptHashNotMatchDELPL
    | ScriptDoesNotValidateDELPL
    deriving (Show, Eq)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delplTransition      ]

delplTransition
  :: forall hashAlgo dsignAlgo vrfAlgo
   . (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (DELPL hashAlgo dsignAlgo vrfAlgo)
delplTransition = do
  TRC (DelplEnv slotIx ptr pp, d, c) <- judgmentContext
  case c of
    RegPool _ -> do
      ps <-
        trans @(POOL hashAlgo dsignAlgo vrfAlgo) $ TRC (PoolEnv slotIx pp, _pstate d, c)
      pure $ d { _pstate = ps }
    RetirePool _ _ -> do
      ps <-
        trans @(POOL hashAlgo dsignAlgo vrfAlgo) $ TRC (PoolEnv slotIx pp, _pstate d, c)
      pure $ d { _pstate = ps }
    GenesisDelegate _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo vrfAlgo) $ TRC (DelegEnv slotIx ptr, _dstate d, c)
      pure $ d { _dstate = ds }

    RegKey _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo vrfAlgo) $ TRC (DelegEnv slotIx ptr, _dstate d, c)
      pure $ d { _dstate = ds }

    DeRegKey _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo vrfAlgo) $ TRC (DelegEnv slotIx ptr, _dstate d, c)
      pure $ d { _dstate = ds }

    Delegate _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo vrfAlgo) $ TRC (DelegEnv slotIx ptr, _dstate d, c)
      pure $ d { _dstate = ds }

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Embed (POOL hashAlgo dsignAlgo vrfAlgo) (DELPL hashAlgo dsignAlgo vrfAlgo)
 where
  wrapFailed = PoolFailure

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Embed (DELEG hashAlgo dsignAlgo vrfAlgo) (DELPL hashAlgo dsignAlgo vrfAlgo)
 where
  wrapFailed = DelegFailure
