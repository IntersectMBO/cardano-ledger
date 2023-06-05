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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Delpl (
  ShelleyDELPL,
  DelplEnv (..),
  ShelleyDelplPredFailure (..),
  ShelleyDelplEvent,
  PredicateFailure,
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase, invalidKey)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyDELPL)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState,
  CertState,
  DState,
  PState,
  certDState,
  certPState,
 )
import Cardano.Ledger.Shelley.Rules.Deleg (DelegEnv (..), ShelleyDELEG, ShelleyDelegPredFailure)
import Cardano.Ledger.Shelley.Rules.Pool (PoolEnv (..), ShelleyPOOL, ShelleyPoolPredFailure)
import Cardano.Ledger.Shelley.TxBody (Ptr)
import Cardano.Ledger.Shelley.TxCert (GenesisDelegCert (..), ShelleyTxCert (..))
import Cardano.Ledger.Slot (SlotNo)
import Control.DeepSeq
import Control.State.Transition
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DelplEnv era = DelplEnv
  { delplSlotNo :: SlotNo
  , delPlPtr :: Ptr
  , delPlPp :: PParams era
  , delPlAcnt :: AccountState
  }

data ShelleyDelplPredFailure era
  = PoolFailure (PredicateFailure (EraRule "POOL" era)) -- Subtransition Failures
  | DelegFailure (PredicateFailure (EraRule "DELEG" era)) -- Subtransition Failures
  deriving (Generic)

data ShelleyDelplEvent era
  = PoolEvent (Event (ShelleyPOOL era))
  | DelegEvent (Event (ShelleyDELEG era))

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELEG" era))
  , Eq (PredicateFailure (EraRule "POOL" era))
  ) =>
  Eq (ShelleyDelplPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELEG" era))
  , Show (PredicateFailure (EraRule "POOL" era))
  ) =>
  Show (ShelleyDelplPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "DELEG" era))
  , NoThunks (PredicateFailure (EraRule "POOL" era))
  ) =>
  NoThunks (ShelleyDelplPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "DELEG" era))
  , NFData (PredicateFailure (EraRule "POOL" era))
  ) =>
  NFData (ShelleyDelplPredFailure era)

instance
  ( Era era
  , Embed (EraRule "DELEG" era) (ShelleyDELPL era)
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , State (EraRule "DELEG" era) ~ DState era
  , Embed (EraRule "POOL" era) (ShelleyDELPL era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , State (EraRule "POOL" era) ~ PState era
  , Signal (EraRule "DELEG" era) ~ TxCert era
  , Embed (EraRule "POOL" era) (ShelleyDELPL era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Signal (EraRule "POOL" era) ~ TxCert era
  , TxCert era ~ ShelleyTxCert era
  ) =>
  STS (ShelleyDELPL era)
  where
  type State (ShelleyDELPL era) = CertState era
  type Signal (ShelleyDELPL era) = TxCert era
  type Environment (ShelleyDELPL era) = DelplEnv era
  type BaseM (ShelleyDELPL era) = ShelleyBase
  type PredicateFailure (ShelleyDELPL era) = ShelleyDelplPredFailure era
  type Event (ShelleyDELPL era) = ShelleyDelplEvent era

  transitionRules = [delplTransition]

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "POOL" era))
  , EncCBOR (PredicateFailure (EraRule "DELEG" era))
  , Typeable (Script era)
  ) =>
  EncCBOR (ShelleyDelplPredFailure era)
  where
  encCBOR = \case
    (PoolFailure a) ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR a
    (DelegFailure a) ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR a

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "POOL" era))
  , DecCBOR (PredicateFailure (EraRule "DELEG" era))
  , Typeable (Script era)
  ) =>
  DecCBOR (ShelleyDelplPredFailure era)
  where
  decCBOR =
    decodeRecordSum
      "PredicateFailure (DELPL era)"
      ( \case
          0 -> do
            a <- decCBOR
            pure (2, PoolFailure a)
          1 -> do
            a <- decCBOR
            pure (2, DelegFailure a)
          k -> invalidKey k
      )

delplTransition ::
  forall era.
  ( Embed (EraRule "DELEG" era) (ShelleyDELPL era)
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , State (EraRule "DELEG" era) ~ DState era
  , State (EraRule "POOL" era) ~ PState era
  , Signal (EraRule "DELEG" era) ~ TxCert era
  , Embed (EraRule "POOL" era) (ShelleyDELPL era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Signal (EraRule "POOL" era) ~ TxCert era
  , TxCert era ~ ShelleyTxCert era
  ) =>
  TransitionRule (ShelleyDELPL era)
delplTransition = do
  TRC (DelplEnv slot ptr pp acnt, d, c) <- judgmentContext
  case c of
    ShelleyTxCertPool (RegPool _) -> do
      ps <-
        trans @(EraRule "POOL" era) $ TRC (PoolEnv slot pp, certPState d, c)
      pure $ d {certPState = ps}
    ShelleyTxCertPool (RetirePool _ _) -> do
      ps <-
        trans @(EraRule "POOL" era) $ TRC (PoolEnv slot pp, certPState d, c)
      pure $ d {certPState = ps}
    ShelleyTxCertGenesisDeleg GenesisDelegCert {} -> do
      ds <-
        trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, certDState d, c)
      pure $ d {certDState = ds}
    ShelleyTxCertDelegCert _ -> do
      ds <-
        trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, certDState d, c)
      pure $ d {certDState = ds}
    ShelleyTxCertMir _ -> do
      ds <- trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, certDState d, c)
      pure $ d {certDState = ds}

instance
  ( Era era
  , STS (ShelleyPOOL era)
  , PredicateFailure (EraRule "POOL" era) ~ ShelleyPoolPredFailure era
  ) =>
  Embed (ShelleyPOOL era) (ShelleyDELPL era)
  where
  wrapFailed = PoolFailure
  wrapEvent = PoolEvent

instance
  ( ShelleyEraTxCert era
  , EraPParams era
  , PredicateFailure (EraRule "DELEG" era) ~ ShelleyDelegPredFailure era
  ) =>
  Embed (ShelleyDELEG era) (ShelleyDELPL era)
  where
  wrapFailed = DelegFailure
  wrapEvent = DelegEvent
