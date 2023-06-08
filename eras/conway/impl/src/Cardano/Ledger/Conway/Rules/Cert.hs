{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Cert (
  ConwayCERT,
  ConwayCertPredFailure (..),
  ConwayCertEvent,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayCERT, ConwayDELEG, ConwayPOOL, ConwayVDEL)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure (..))
import Cardano.Ledger.Conway.Rules.VDel (ConwayVDelPredFailure, VDelEnv (VDelEnv))
import Cardano.Ledger.Conway.TxCert (ConwayCommitteeCert, ConwayDelegCert, ConwayTxCert (..))
import Cardano.Ledger.Shelley.API (CertState (..), DState, DelegEnv (DelegEnv), DelplEnv (DelplEnv), PState, PoolEnv (PoolEnv), VState)
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (Embed, STS (..), TRC (TRC), TransitionRule, judgmentContext, trans, wrapEvent, wrapFailed)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data ConwayCertPredFailure era
  = DelegFailure (PredicateFailure (EraRule "DELEG" era))
  | PoolFailure (PredicateFailure (EraRule "POOL" era))
  | VDelFailure (PredicateFailure (EraRule "VDEL" era))
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELEG" era))
  , Show (PredicateFailure (EraRule "POOL" era))
  , Show (PredicateFailure (EraRule "VDEL" era))
  ) =>
  Show (ConwayCertPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELEG" era))
  , Eq (PredicateFailure (EraRule "POOL" era))
  , Eq (PredicateFailure (EraRule "VDEL" era))
  ) =>
  Eq (ConwayCertPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "DELEG" era))
  , NoThunks (PredicateFailure (EraRule "POOL" era))
  , NoThunks (PredicateFailure (EraRule "VDEL" era))
  ) =>
  NoThunks (ConwayCertPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "DELEG" era))
  , NFData (PredicateFailure (EraRule "POOL" era))
  , NFData (PredicateFailure (EraRule "VDEL" era))
  ) =>
  NFData (ConwayCertPredFailure era)

data ConwayCertEvent era
  = DelegEvent (Event (ConwayDELEG era))
  | PoolEvent (Event (ConwayPOOL era))
  | VDelEvent (Event (ConwayVDEL era))

instance
  forall era.
  ( Era era
  , State (EraRule "DELEG" era) ~ DState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "VDEL" era) ~ VState era
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "VDEL" era) ~ VDelEnv era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert (EraCrypto era)
  , Signal (EraRule "POOL" era) ~ PoolCert (EraCrypto era)
  , Signal (EraRule "VDEL" era) ~ ConwayCommitteeCert (EraCrypto era)
  , Embed (EraRule "DELEG" era) (ConwayCERT era)
  , Embed (EraRule "POOL" era) (ConwayCERT era)
  , Embed (EraRule "VDEL" era) (ConwayCERT era)
  , TxCert era ~ ConwayTxCert era
  ) =>
  STS (ConwayCERT era)
  where
  type State (ConwayCERT era) = CertState era
  type Signal (ConwayCERT era) = TxCert era
  type Environment (ConwayCERT era) = DelplEnv era
  type BaseM (ConwayCERT era) = ShelleyBase
  type PredicateFailure (ConwayCERT era) = ConwayCertPredFailure era
  type Event (ConwayCERT era) = ConwayCertEvent era

  transitionRules = [certTransition @era]

certTransition ::
  forall era.
  ( State (EraRule "DELEG" era) ~ DState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "VDEL" era) ~ VState era
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "VDEL" era) ~ VDelEnv era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert (EraCrypto era)
  , Signal (EraRule "POOL" era) ~ PoolCert (EraCrypto era)
  , Signal (EraRule "VDEL" era) ~ ConwayCommitteeCert (EraCrypto era)
  , Embed (EraRule "DELEG" era) (ConwayCERT era)
  , Embed (EraRule "POOL" era) (ConwayCERT era)
  , Embed (EraRule "VDEL" era) (ConwayCERT era)
  , TxCert era ~ ConwayTxCert era
  ) =>
  TransitionRule (ConwayCERT era)
certTransition = do
  TRC (DelplEnv slot ptr pp acnt, cState@CertState {certDState, certPState, certVState}, c) <- judgmentContext
  case c of
    ConwayTxCertDeleg delegCert -> do
      newDState <- trans @(EraRule "DELEG" era) $ TRC (DelegEnv slot ptr acnt pp, certDState, delegCert)
      pure $ cState {certDState = newDState}
    ConwayTxCertPool poolCert -> do
      newPState <- trans @(EraRule "POOL" era) $ TRC (PoolEnv slot pp, certPState, poolCert)
      pure $ cState {certPState = newPState}
    ConwayTxCertCommittee committeeCert -> do
      newVState <- trans @(EraRule "VDEL" era) $ TRC (VDelEnv, certVState, committeeCert)
      pure $ cState {certVState = newVState}

instance
  ( Era era
  , STS (ConwayDELEG era)
  , PredicateFailure (EraRule "DELEG" era) ~ ConwayDelegPredFailure era
  ) =>
  Embed (ConwayDELEG era) (ConwayCERT era)
  where
  wrapFailed = DelegFailure
  wrapEvent = DelegEvent

instance
  ( Era era
  , STS (ConwayPOOL era)
  , PredicateFailure (EraRule "POOL" era) ~ ShelleyPoolPredFailure era
  , PredicateFailure (ConwayPOOL era) ~ ShelleyPoolPredFailure era
  , BaseM (ConwayPOOL era) ~ ShelleyBase
  ) =>
  Embed (ConwayPOOL era) (ConwayCERT era)
  where
  wrapFailed = PoolFailure
  wrapEvent = PoolEvent

instance
  ( Era era
  , STS (ConwayVDEL era)
  , PredicateFailure (EraRule "VDEL" era) ~ ConwayVDelPredFailure era
  ) =>
  Embed (ConwayVDEL era) (ConwayCERT era)
  where
  wrapFailed = VDelFailure
  wrapEvent = VDelEvent

instance
  ( Typeable era
  , EncCBOR (PredicateFailure (EraRule "DELEG" era))
  , EncCBOR (PredicateFailure (EraRule "POOL" era))
  , EncCBOR (PredicateFailure (EraRule "VDEL" era))
  ) =>
  EncCBOR (ConwayCertPredFailure era)
  where
  encCBOR =
    encode . \case
      DelegFailure x -> Sum (DelegFailure @era) 1 !> To x
      PoolFailure x -> Sum (PoolFailure @era) 2 !> To x
      VDelFailure x -> Sum (VDelFailure @era) 3 !> To x

instance
  ( Typeable era
  , DecCBOR (PredicateFailure (EraRule "DELEG" era))
  , DecCBOR (PredicateFailure (EraRule "POOL" era))
  , DecCBOR (PredicateFailure (EraRule "VDEL" era))
  ) =>
  DecCBOR (ConwayCertPredFailure era)
  where
  decCBOR =
    decode $ Summands "ConwayCertPredFailure" $ \case
      1 -> SumD DelegFailure <! From
      2 -> SumD PoolFailure <! From
      3 -> SumD VDelFailure <! From
      n -> Invalid n
