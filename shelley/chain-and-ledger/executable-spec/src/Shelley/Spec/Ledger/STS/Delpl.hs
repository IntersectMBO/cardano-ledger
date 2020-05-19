{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Delpl
  ( DELPL,
    DelplEnv (..),
    PredicateFailure (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord,
    encodeListLen,
    matchSize,
  )
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, invalidKey)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState,
    _dstate,
    _pstate,
    emptyDelegation,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Deleg (DELEG, DelegEnv (..))
import Shelley.Spec.Ledger.STS.Pool (POOL, PoolEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.TxData
  ( DCert (..),
    DelegCert (..),
    GenesisDelegCert (..),
    PoolCert (..),
    Ptr,
  )

data DELPL crypto

data DelplEnv = DelplEnv
  { delplSlotNo :: SlotNo,
    delPlPtr :: Ptr,
    delPlPp :: PParams,
    delPlAcnt :: AccountState
  }

instance
  Crypto crypto =>
  STS (DELPL crypto)
  where
  type State (DELPL crypto) = DPState crypto
  type Signal (DELPL crypto) = DCert crypto
  type Environment (DELPL crypto) = DelplEnv
  type BaseM (DELPL crypto) = ShelleyBase
  data PredicateFailure (DELPL crypto)
    = PoolFailure (PredicateFailure (POOL crypto)) -- Subtransition Failures
    | DelegFailure (PredicateFailure (DELEG crypto)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = [pure emptyDelegation]
  transitionRules = [delplTransition]

instance NoUnexpectedThunks (PredicateFailure (DELPL crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (DELPL crypto))
  where
  toCBOR = \case
    (PoolFailure a) ->
      encodeListLen 2 <> toCBOR (0 :: Word8)
        <> toCBOR a
    (DelegFailure a) ->
      encodeListLen 2 <> toCBOR (1 :: Word8)
        <> toCBOR a

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (DELPL crypto))
  where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "PoolFailure" 2 n
        a <- fromCBOR
        pure $ PoolFailure a
      1 -> do
        matchSize "DelegFailure" 2 n
        a <- fromCBOR
        pure $ DelegFailure a
      k -> invalidKey k

delplTransition ::
  forall crypto.
  Crypto crypto =>
  TransitionRule (DELPL crypto)
delplTransition = do
  TRC (DelplEnv slot ptr pp acnt, d, c) <- judgmentContext
  case c of
    DCertPool (RegPool _) -> do
      ps <-
        trans @(POOL crypto) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d {_pstate = ps}
    DCertPool (RetirePool _ _) -> do
      ps <-
        trans @(POOL crypto) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d {_pstate = ps}
    DCertGenesis (GenesisDelegCert {}) -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (RegKey _) -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (DeRegKey _) -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (Delegate _) -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertMir _ -> do
      ds <- trans @(DELEG crypto) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}

instance
  Crypto crypto =>
  Embed (POOL crypto) (DELPL crypto)
  where
  wrapFailed = PoolFailure

instance
  Crypto crypto =>
  Embed (DELEG crypto) (DELPL crypto)
  where
  wrapFailed = DelegFailure
