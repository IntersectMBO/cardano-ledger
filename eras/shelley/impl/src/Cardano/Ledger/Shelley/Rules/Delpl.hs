{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Cardano.Ledger.Shelley.Rules.Delpl
  ( DELPL,
    DelplEnv (..),
    DelplPredicateFailure (..),
    DelplEvent,
    PredicateFailure,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase, invalidKey)
import Cardano.Ledger.Core
import Cardano.Ledger.Serialization (decodeRecordSum)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    DPState,
    DState,
    PState,
    dpsDState,
    dpsPState,
  )
import Cardano.Ledger.Shelley.Rules.Deleg (DELEG, DelegEnv (..), DelegPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Pool (POOL, PoolEnv (..), PoolPredicateFailure)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    GenesisDelegCert (..),
    PoolCert (..),
    Ptr,
  )
import Cardano.Ledger.Slot (SlotNo)
import Control.State.Transition
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import NoThunks.Class (NoThunks (..))

data DELPL era

data DelplEnv era = DelplEnv
  { delplSlotNo :: SlotNo,
    delPlPtr :: Ptr,
    delPlPp :: PParams era,
    delPlAcnt :: AccountState
  }

data DelplPredicateFailure era
  = PoolFailure (PredicateFailure (EraRule "POOL" era)) -- Subtransition Failures
  | DelegFailure (PredicateFailure (EraRule "DELEG" era)) -- Subtransition Failures
  deriving (Generic)

data DelplEvent era
  = PoolEvent (Event (POOL era))
  | DelegEvent (Event (DELEG era))

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELEG" era)),
    Eq (PredicateFailure (EraRule "POOL" era))
  ) =>
  Eq (DelplPredicateFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELEG" era)),
    Show (PredicateFailure (EraRule "POOL" era))
  ) =>
  Show (DelplPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "DELEG" era)),
    NoThunks (PredicateFailure (EraRule "POOL" era))
  ) =>
  NoThunks (DelplPredicateFailure era)

instance
  ( Era era,
    Embed (EraRule "DELEG" era) (DELPL era),
    Environment (EraRule "DELEG" era) ~ DelegEnv era,
    State (EraRule "DELEG" era) ~ DState (Crypto era),
    Signal (EraRule "DELEG" era) ~ DCert (Crypto era),
    Embed (EraRule "POOL" era) (DELPL era),
    Environment (EraRule "POOL" era) ~ PoolEnv era,
    State (EraRule "POOL" era) ~ PState (Crypto era),
    Signal (EraRule "POOL" era) ~ DCert (Crypto era)
  ) =>
  STS (DELPL era)
  where
  type State (DELPL era) = DPState (Crypto era)
  type Signal (DELPL era) = DCert (Crypto era)
  type Environment (DELPL era) = DelplEnv era
  type BaseM (DELPL era) = ShelleyBase
  type PredicateFailure (DELPL era) = DelplPredicateFailure era
  type Event (DELPL era) = DelplEvent era

  transitionRules = [delplTransition]

instance
  ( Era era,
    ToCBOR (PredicateFailure (EraRule "POOL" era)),
    ToCBOR (PredicateFailure (EraRule "DELEG" era)),
    Typeable (Script era)
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
    FromCBOR (PredicateFailure (EraRule "POOL" era)),
    FromCBOR (PredicateFailure (EraRule "DELEG" era)),
    Typeable (Script era)
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
  ( Embed (EraRule "DELEG" era) (DELPL era),
    Environment (EraRule "DELEG" era) ~ DelegEnv era,
    State (EraRule "DELEG" era) ~ DState (Crypto era),
    Signal (EraRule "DELEG" era) ~ DCert (Crypto era),
    Embed (EraRule "POOL" era) (DELPL era),
    Environment (EraRule "POOL" era) ~ PoolEnv era,
    State (EraRule "POOL" era) ~ PState (Crypto era),
    Signal (EraRule "POOL" era) ~ DCert (Crypto era)
  ) =>
  TransitionRule (DELPL era)
delplTransition = do
  TRC (DelplEnv slot ptr pp acnt, d, c) <- judgmentContext
  case c of
    DCertPool (RegPool _) -> do
      ps <-
        trans @(EraRule "POOL" era) $ TRC (PoolEnv slot pp, dpsPState d, c)
      pure $ d {dpsPState = ps}
    DCertPool (RetirePool _ _) -> do
      ps <-
        trans @(EraRule "POOL" era) $ TRC (PoolEnv slot pp, dpsPState d, c)
      pure $ d {dpsPState = ps}
    DCertGenesis GenesisDelegCert {} -> do
      ds <-
        trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, dpsDState d, c)
      pure $ d {dpsDState = ds}
    DCertDeleg (RegKey _) -> do
      ds <-
        trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, dpsDState d, c)
      pure $ d {dpsDState = ds}
    DCertDeleg (DeRegKey _) -> do
      ds <-
        trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, dpsDState d, c)
      pure $ d {dpsDState = ds}
    DCertDeleg (Delegate _) -> do
      ds <-
        trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, dpsDState d, c)
      pure $ d {dpsDState = ds}
    DCertMir _ -> do
      ds <- trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, dpsDState d, c)
      pure $ d {dpsDState = ds}

instance
  ( Era era,
    STS (POOL era),
    PredicateFailure (EraRule "POOL" era) ~ PoolPredicateFailure era
  ) =>
  Embed (POOL era) (DELPL era)
  where
  wrapFailed = PoolFailure
  wrapEvent = PoolEvent

instance
  ( Era era,
    HasField "_protocolVersion" (PParams era) ProtVer,
    PredicateFailure (EraRule "DELEG" era) ~ DelegPredicateFailure era
  ) =>
  Embed (DELEG era) (DELPL era)
  where
  wrapFailed = DelegFailure
  wrapEvent = DelegEvent
