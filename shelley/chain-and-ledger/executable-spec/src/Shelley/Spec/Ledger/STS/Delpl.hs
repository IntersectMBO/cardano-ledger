{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
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
    DelplPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import Control.State.Transition
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, invalidKey)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState,
    emptyDelegation,
    _dstate,
    _pstate,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Deleg (DELEG, DelegEnv (..))
import Shelley.Spec.Ledger.STS.Pool (POOL, PoolEnv (..))
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    GenesisDelegCert (..),
    PoolCert (..),
    Ptr,
  )

data DELPL era

data DelplEnv era = DelplEnv
  { delplSlotNo :: SlotNo,
    delPlPtr :: Ptr,
    delPlPp :: PParams era,
    delPlAcnt :: AccountState
  }

data DelplPredicateFailure era
  = PoolFailure (PredicateFailure (POOL era)) -- Subtransition Failures
  | DelegFailure (PredicateFailure (DELEG era)) -- Subtransition Failures
  deriving (Show, Eq, Generic)

instance
  Era era =>
  STS (DELPL era)
  where
  type State (DELPL era) = DPState era
  type Signal (DELPL era) = DCert era
  type Environment (DELPL era) = DelplEnv era
  type BaseM (DELPL era) = ShelleyBase
  type PredicateFailure (DELPL era) = DelplPredicateFailure era

  initialRules = [pure emptyDelegation]
  transitionRules = [delplTransition]

instance NoThunks (DelplPredicateFailure era)

instance
  (Typeable era, Era era, Typeable (Core.Script era)) =>
  ToCBOR (DelplPredicateFailure era)
  where
  toCBOR = \case
    (PoolFailure a) ->
      encodeListLen 2 <> toCBOR (0 :: Word8)
        <> toCBOR a
    (DelegFailure a) ->
      encodeListLen 2 <> toCBOR (1 :: Word8)
        <> toCBOR a

instance
  (Era era, Typeable (Core.Script era)) =>
  FromCBOR (DelplPredicateFailure era)
  where
  fromCBOR =
    decodeRecordSum
      "PredicateFailure (DELPL era)"
      ( \case
          0 -> do
            a <- fromCBOR
            pure (2, PoolFailure a)
          1 -> do
            a <- fromCBOR
            pure (2, DelegFailure a)
          k -> invalidKey k
      )

delplTransition ::
  forall era.
  Era era =>
  TransitionRule (DELPL era)
delplTransition = do
  TRC (DelplEnv slot ptr pp acnt, d, c) <- judgmentContext
  case c of
    DCertPool (RegPool _) -> do
      ps <-
        trans @(POOL era) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d {_pstate = ps}
    DCertPool (RetirePool _ _) -> do
      ps <-
        trans @(POOL era) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d {_pstate = ps}
    DCertGenesis (GenesisDelegCert {}) -> do
      ds <-
        trans @(DELEG era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (RegKey _) -> do
      ds <-
        trans @(DELEG era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (DeRegKey _) -> do
      ds <-
        trans @(DELEG era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (Delegate _) -> do
      ds <-
        trans @(DELEG era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertMir _ -> do
      ds <- trans @(DELEG era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}

instance
  Era era =>
  Embed (POOL era) (DELPL era)
  where
  wrapFailed = PoolFailure

instance
  Era era =>
  Embed (DELEG era) (DELPL era)
  where
  wrapFailed = DelegFailure
