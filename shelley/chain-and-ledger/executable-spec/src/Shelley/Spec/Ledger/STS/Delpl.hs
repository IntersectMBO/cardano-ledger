{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Cardano.Ledger.Era (Crypto, Era)
import Control.State.Transition
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, invalidKey)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState,
    DState,
    PState,
    _dstate,
    _pstate,
  )
import Shelley.Spec.Ledger.STS.Deleg (DELEG, DelegEnv (..), DelegPredicateFailure)
import Shelley.Spec.Ledger.STS.Pool (POOL, PoolEnv (..), PoolPredicateFailure)
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
    delPlPp :: Core.PParams era,
    delPlAcnt :: AccountState
  }

data DelplPredicateFailure era
  = PoolFailure (PredicateFailure (Core.EraRule "POOL" era)) -- Subtransition Failures
  | DelegFailure (PredicateFailure (Core.EraRule "DELEG" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Eq (PredicateFailure (Core.EraRule "DELEG" era)),
    Eq (PredicateFailure (Core.EraRule "POOL" era))
  ) =>
  Eq (DelplPredicateFailure era)

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "DELEG" era)),
    Show (PredicateFailure (Core.EraRule "POOL" era))
  ) =>
  Show (DelplPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (Core.EraRule "DELEG" era)),
    NoThunks (PredicateFailure (Core.EraRule "POOL" era))
  ) =>
  NoThunks (DelplPredicateFailure era)

instance
  ( Era era,
    Embed (Core.EraRule "DELEG" era) (DELPL era),
    Environment (Core.EraRule "DELEG" era) ~ DelegEnv,
    State (Core.EraRule "DELEG" era) ~ DState (Crypto era),
    Signal (Core.EraRule "DELEG" era) ~ DCert (Crypto era),
    Embed (Core.EraRule "POOL" era) (DELPL era),
    Environment (Core.EraRule "POOL" era) ~ PoolEnv era,
    State (Core.EraRule "POOL" era) ~ PState (Crypto era),
    Signal (Core.EraRule "POOL" era) ~ DCert (Crypto era)
  ) =>
  STS (DELPL era)
  where
  type State (DELPL era) = DPState (Crypto era)
  type Signal (DELPL era) = DCert (Crypto era)
  type Environment (DELPL era) = DelplEnv era
  type BaseM (DELPL era) = ShelleyBase
  type PredicateFailure (DELPL era) = DelplPredicateFailure era

  transitionRules = [delplTransition]

instance
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "POOL" era)),
    ToCBOR (PredicateFailure (Core.EraRule "DELEG" era)),
    Typeable (Core.Script era)
  ) =>
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
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "POOL" era)),
    FromCBOR (PredicateFailure (Core.EraRule "DELEG" era)),
    Typeable (Core.Script era)
  ) =>
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
  ( Embed (Core.EraRule "DELEG" era) (DELPL era),
    Environment (Core.EraRule "DELEG" era) ~ DelegEnv,
    State (Core.EraRule "DELEG" era) ~ DState (Crypto era),
    Signal (Core.EraRule "DELEG" era) ~ DCert (Crypto era),
    Embed (Core.EraRule "POOL" era) (DELPL era),
    Environment (Core.EraRule "POOL" era) ~ PoolEnv era,
    State (Core.EraRule "POOL" era) ~ PState (Crypto era),
    Signal (Core.EraRule "POOL" era) ~ DCert (Crypto era)
  ) =>
  TransitionRule (DELPL era)
delplTransition = do
  TRC (DelplEnv slot ptr pp acnt, d, c) <- judgmentContext
  case c of
    DCertPool (RegPool _) -> do
      ps <-
        trans @(Core.EraRule "POOL" era) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d {_pstate = ps}
    DCertPool (RetirePool _ _) -> do
      ps <-
        trans @(Core.EraRule "POOL" era) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d {_pstate = ps}
    DCertGenesis (GenesisDelegCert {}) -> do
      ds <-
        trans @(Core.EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (RegKey _) -> do
      ds <-
        trans @(Core.EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (DeRegKey _) -> do
      ds <-
        trans @(Core.EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (Delegate _) -> do
      ds <-
        trans @(Core.EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertMir _ -> do
      ds <- trans @(Core.EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt, _dstate d, c)
      pure $ d {_dstate = ds}

instance
  ( Era era,
    STS (POOL era),
    PredicateFailure (Core.EraRule "POOL" era) ~ PoolPredicateFailure era
  ) =>
  Embed (POOL era) (DELPL era)
  where
  wrapFailed = PoolFailure

instance
  ( Era era,
    PredicateFailure (Core.EraRule "DELEG" era) ~ DelegPredicateFailure era
  ) =>
  Embed (DELEG era) (DELPL era)
  where
  wrapFailed = DelegFailure
