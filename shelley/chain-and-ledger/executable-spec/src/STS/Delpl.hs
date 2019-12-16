{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Delpl
  ( DELPL
  , DelplEnv (..)
  , PredicateFailure(..)
  )
where

import           BaseTypes
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin (Coin)
import           Control.State.Transition
import           Delegation.Certificates
import           GHC.Generics (Generic)
import           LedgerState
import           PParams hiding (d)
import           Slot
import           STS.Deleg
import           STS.Pool
import           TxData

data DELPL crypto

data DelplEnv
  = DelplEnv
    { delplSlotNo   :: SlotNo
    , delPlPtr      :: Ptr
    , delPlPp       :: PParams
    , delPlReserves :: Coin
    }

instance
  Crypto crypto
  => STS (DELPL crypto)
 where
  type State (DELPL crypto)       = DPState crypto
  type Signal (DELPL crypto)      = DCert crypto
  type Environment (DELPL crypto) = DelplEnv
  type BaseM (DELPL crypto) = ShelleyBase
  data PredicateFailure (DELPL crypto)
    = PoolFailure (PredicateFailure (POOL crypto))
    | DelegFailure (PredicateFailure (DELEG crypto))
    | ScriptNotInWitnessDELPL
    | ScriptHashNotMatchDELPL
    | ScriptDoesNotValidateDELPL
    deriving (Show, Eq, Generic)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delplTransition      ]

instance NoUnexpectedThunks (PredicateFailure (DELPL crypto))

delplTransition
  :: forall crypto
   . Crypto crypto
  => TransitionRule (DELPL crypto)
delplTransition = do
  TRC (DelplEnv slotIx _ptr pp _reserves, d, c) <- judgmentContext
  case c of
    RegPool _ -> do
      ps <-
        trans @(POOL crypto) $ TRC (PoolEnv slotIx pp, _pstate d, c)
      pure $ d { _pstate = ps }
    RetirePool _ _ -> do
      ps <-
        trans @(POOL crypto) $ TRC (PoolEnv slotIx pp, _pstate d, c)
      pure $ d { _pstate = ps }
    GenesisDelegate _ -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slotIx _ptr _reserves, _dstate d, c)
      pure $ d { _dstate = ds }

    RegKey _ -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slotIx _ptr _reserves, _dstate d, c)
      pure $ d { _dstate = ds }

    DeRegKey _ -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slotIx _ptr _reserves, _dstate d, c)
      pure $ d { _dstate = ds }

    Delegate _ -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slotIx _ptr _reserves , _dstate d, c)
      pure $ d { _dstate = ds }

    InstantaneousRewards _ -> do
      ds <- trans @(DELEG crypto) $ TRC (DelegEnv slotIx _ptr _reserves , _dstate d, c)
      pure $ d { _dstate = ds }


instance
  Crypto crypto
  => Embed (POOL crypto) (DELPL crypto)
 where
  wrapFailed = PoolFailure

instance
  Crypto crypto
  => Embed (DELEG crypto) (DELPL crypto)
 where
  wrapFailed = DelegFailure
