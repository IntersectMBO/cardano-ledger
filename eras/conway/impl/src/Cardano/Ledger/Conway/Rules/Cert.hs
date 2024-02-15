{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  CertEnv (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo, ShelleyBase, SlotNo)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (
  ConwayCERT,
  ConwayDELEG,
  ConwayEra,
  ConwayGOVCERT,
 )
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegEvent, ConwayDelegPredFailure (..))
import Cardano.Ledger.Conway.Rules.GovCert (
  ConwayGovCertEnv (..),
  ConwayGovCertEvent,
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert,
  ConwayGovCert,
  ConwayTxCert (..),
 )
import Cardano.Ledger.Shelley.API (
  CertState (..),
  DState,
  PState,
  PoolEnv (PoolEnv),
  VState,
 )
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPOOL, ShelleyPoolPredFailure)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  Embed,
  STS (..),
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  trans,
  wrapEvent,
  wrapFailed,
 )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data CertEnv era = CertEnv
  { ceSlotNo :: !SlotNo
  , cePParams :: !(PParams era)
  , ceCurrentEpoch :: !EpochNo
  }
  deriving (Generic)

deriving instance Eq (PParams era) => Eq (CertEnv era)
deriving instance Show (PParams era) => Show (CertEnv era)

data ConwayCertPredFailure era
  = DelegFailure (PredicateFailure (EraRule "DELEG" era))
  | PoolFailure (PredicateFailure (EraRule "POOL" era))
  | GovCertFailure (PredicateFailure (EraRule "GOVCERT" era))
  deriving (Generic)

type instance EraRuleFailure "CERT" (ConwayEra c) = ConwayCertPredFailure (ConwayEra c)

instance InjectRuleFailure "CERT" ConwayCertPredFailure (ConwayEra c)

instance InjectRuleFailure "CERT" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = DelegFailure

instance InjectRuleFailure "CERT" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = PoolFailure

instance InjectRuleFailure "CERT" ConwayGovCertPredFailure (ConwayEra c) where
  injectFailure = GovCertFailure

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELEG" era))
  , Show (PredicateFailure (EraRule "POOL" era))
  , Show (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  Show (ConwayCertPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELEG" era))
  , Eq (PredicateFailure (EraRule "POOL" era))
  , Eq (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  Eq (ConwayCertPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "DELEG" era))
  , NoThunks (PredicateFailure (EraRule "POOL" era))
  , NoThunks (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  NoThunks (ConwayCertPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "DELEG" era))
  , NFData (PredicateFailure (EraRule "POOL" era))
  , NFData (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  NFData (ConwayCertPredFailure era)

data ConwayCertEvent era
  = DelegEvent (Event (EraRule "DELEG" era))
  | PoolEvent (Event (EraRule "POOL" era))
  | GovCertEvent (Event (EraRule "GOVCERT" era))

instance
  forall era.
  ( Era era
  , State (EraRule "DELEG" era) ~ DState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "GOVCERT" era) ~ VState era
  , Environment (EraRule "DELEG" era) ~ PParams era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "GOVCERT" era) ~ ConwayGovCertEnv era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert (EraCrypto era)
  , Signal (EraRule "POOL" era) ~ PoolCert (EraCrypto era)
  , Signal (EraRule "GOVCERT" era) ~ ConwayGovCert (EraCrypto era)
  , Embed (EraRule "DELEG" era) (ConwayCERT era)
  , Embed (EraRule "POOL" era) (ConwayCERT era)
  , Embed (EraRule "GOVCERT" era) (ConwayCERT era)
  , TxCert era ~ ConwayTxCert era
  ) =>
  STS (ConwayCERT era)
  where
  type State (ConwayCERT era) = CertState era
  type Signal (ConwayCERT era) = TxCert era
  type Environment (ConwayCERT era) = CertEnv era
  type BaseM (ConwayCERT era) = ShelleyBase
  type PredicateFailure (ConwayCERT era) = ConwayCertPredFailure era
  type Event (ConwayCERT era) = ConwayCertEvent era

  transitionRules = [certTransition @era]

certTransition ::
  forall era.
  ( State (EraRule "DELEG" era) ~ DState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "GOVCERT" era) ~ VState era
  , Environment (EraRule "DELEG" era) ~ PParams era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "GOVCERT" era) ~ ConwayGovCertEnv era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert (EraCrypto era)
  , Signal (EraRule "POOL" era) ~ PoolCert (EraCrypto era)
  , Signal (EraRule "GOVCERT" era) ~ ConwayGovCert (EraCrypto era)
  , Embed (EraRule "DELEG" era) (ConwayCERT era)
  , Embed (EraRule "POOL" era) (ConwayCERT era)
  , Embed (EraRule "GOVCERT" era) (ConwayCERT era)
  , TxCert era ~ ConwayTxCert era
  ) =>
  TransitionRule (ConwayCERT era)
certTransition = do
  TRC (CertEnv slot pp currentEpoch, cState, c) <- judgmentContext
  let CertState {certDState, certPState, certVState} = cState
  case c of
    ConwayTxCertDeleg delegCert -> do
      newDState <- trans @(EraRule "DELEG" era) $ TRC (pp, certDState, delegCert)
      pure $ cState {certDState = newDState}
    ConwayTxCertPool poolCert -> do
      newPState <- trans @(EraRule "POOL" era) $ TRC (PoolEnv slot pp, certPState, poolCert)
      pure $ cState {certPState = newPState}
    ConwayTxCertGov govCert -> do
      newVState <- trans @(EraRule "GOVCERT" era) $ TRC (ConwayGovCertEnv pp currentEpoch, certVState, govCert)
      pure $ cState {certVState = newVState}

instance
  ( Era era
  , STS (ConwayDELEG era)
  , Event (EraRule "DELEG" era) ~ ConwayDelegEvent era
  , PredicateFailure (EraRule "DELEG" era) ~ ConwayDelegPredFailure era
  ) =>
  Embed (ConwayDELEG era) (ConwayCERT era)
  where
  wrapFailed = DelegFailure
  wrapEvent = DelegEvent

instance
  ( Era era
  , STS (ShelleyPOOL era)
  , Event (EraRule "POOL" era) ~ PoolEvent era
  , PredicateFailure (EraRule "POOL" era) ~ ShelleyPoolPredFailure era
  , PredicateFailure (ShelleyPOOL era) ~ ShelleyPoolPredFailure era
  , BaseM (ShelleyPOOL era) ~ ShelleyBase
  ) =>
  Embed (ShelleyPOOL era) (ConwayCERT era)
  where
  wrapFailed = PoolFailure
  wrapEvent = PoolEvent

instance
  ( Era era
  , STS (ConwayGOVCERT era)
  , Event (EraRule "GOVCERT" era) ~ ConwayGovCertEvent era
  , PredicateFailure (EraRule "GOVCERT" era) ~ ConwayGovCertPredFailure era
  ) =>
  Embed (ConwayGOVCERT era) (ConwayCERT era)
  where
  wrapFailed = GovCertFailure
  wrapEvent = GovCertEvent

instance
  ( Typeable era
  , EncCBOR (PredicateFailure (EraRule "DELEG" era))
  , EncCBOR (PredicateFailure (EraRule "POOL" era))
  , EncCBOR (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  EncCBOR (ConwayCertPredFailure era)
  where
  encCBOR =
    encode . \case
      DelegFailure x -> Sum (DelegFailure @era) 1 !> To x
      PoolFailure x -> Sum (PoolFailure @era) 2 !> To x
      GovCertFailure x -> Sum (GovCertFailure @era) 3 !> To x

instance
  ( Typeable era
  , DecCBOR (PredicateFailure (EraRule "DELEG" era))
  , DecCBOR (PredicateFailure (EraRule "POOL" era))
  , DecCBOR (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  DecCBOR (ConwayCertPredFailure era)
  where
  decCBOR =
    decode $ Summands "ConwayCertPredFailure" $ \case
      1 -> SumD DelegFailure <! From
      2 -> SumD PoolFailure <! From
      3 -> SumD GovCertFailure <! From
      n -> Invalid n
