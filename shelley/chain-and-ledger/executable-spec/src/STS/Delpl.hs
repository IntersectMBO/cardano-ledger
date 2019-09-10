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

data DELPL hashAlgo dsignAlgo

data DelplEnv
  = DelplEnv Slot Ptr PParams

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (DELPL hashAlgo dsignAlgo)
 where
  type State (DELPL hashAlgo dsignAlgo)       = DPState hashAlgo dsignAlgo
  type Signal (DELPL hashAlgo dsignAlgo)      = DCert hashAlgo dsignAlgo
  type Environment (DELPL hashAlgo dsignAlgo) = DelplEnv
  data PredicateFailure (DELPL hashAlgo dsignAlgo)
    = PoolFailure (PredicateFailure (POOL hashAlgo dsignAlgo))
    | DelegFailure (PredicateFailure (DELEG hashAlgo dsignAlgo))
    | ScriptNotInWitnessDELPL
    | ScriptHashNotMatchDELPL
    | ScriptDoesNotValidateDELPL
    deriving (Show, Eq)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delplTransition      ]

delplTransition
  :: forall hashAlgo dsignAlgo
   . (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (DELPL hashAlgo dsignAlgo)
delplTransition = do
  TRC (DelplEnv slotIx ptr pp, d, c) <- judgmentContext
  case c of
    RegPool _ -> do
      ps <-
        trans @(POOL hashAlgo dsignAlgo) $ TRC (PoolEnv slotIx pp, _pstate d, c)
      pure $ d { _pstate = ps }
    RetirePool _ _ -> do
      ps <-
        trans @(POOL hashAlgo dsignAlgo) $ TRC (PoolEnv slotIx pp, _pstate d, c)
      pure $ d { _pstate = ps }
    GenesisDelegate _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo) $ TRC (DelegEnv slotIx ptr, _dstate d, c)
      pure $ d { _dstate = ds }

    RegKey _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo) $ TRC (DelegEnv slotIx ptr, _dstate d, c)
      pure $ d { _dstate = ds }

    DeRegKey _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo) $ TRC (DelegEnv slotIx ptr, _dstate d, c)
      pure $ d { _dstate = ds }

    Delegate _ -> do
      ds <-
        trans @(DELEG hashAlgo dsignAlgo) $ TRC (DelegEnv slotIx ptr, _dstate d, c)
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
