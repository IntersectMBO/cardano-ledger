{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Cert (
  DijkstraCERT,
  DijkstraCertPredFailure (..),
  DijkstraCertEvent (..),
  CertEnv (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo, ShelleyBase, StrictMaybe)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (
  DijkstraCERT,
  DijkstraDELEG,
  DijkstraEra,
  DijkstraGOVCERT,
 )
import Cardano.Ledger.Dijkstra.Governance (
  Committee,
  GovActionPurpose (..),
  GovActionState,
  GovPurposeId,
 )
import Cardano.Ledger.Dijkstra.Rules.Deleg (
  DijkstraDelegEnv (..),
  DijkstraDelegPredFailure (..),
 )
import Cardano.Ledger.Dijkstra.Rules.GovCert (
  DijkstraGovCertEnv (..),
  DijkstraGovCertPredFailure,
 )
import Cardano.Ledger.Dijkstra.TxCert (
  DijkstraDelegCert,
  DijkstraGovCert,
  DijkstraTxCert (..),
 )
import Cardano.Ledger.Shelley.API (
  PState (..),
  PoolEnv (PoolEnv),
 )
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPOOL, ShelleyPoolPredFailure)
import Cardano.Ledger.State (EraCertState (..))
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
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Data.Void (absurd)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (NoThunks)

data CertEnv era = CertEnv
  { cePParams :: PParams era
  , ceCurrentEpoch :: EpochNo
  -- ^ Lazy on purpose, because not all certificates need to know the current EpochNo
  , ceCurrentCommittee :: StrictMaybe (Committee era)
  , ceCommitteeProposals :: Map.Map (GovPurposeId 'CommitteePurpose era) (GovActionState era)
  }
  deriving (Generic)

instance EraPParams era => EncCBOR (CertEnv era) where
  encCBOR x@(CertEnv _ _ _ _) =
    let CertEnv {..} = x
     in encode $
          Rec CertEnv
            !> To cePParams
            !> To ceCurrentEpoch
            !> To ceCurrentCommittee
            !> To ceCommitteeProposals

deriving instance EraPParams era => Eq (CertEnv era)
deriving instance EraPParams era => Show (CertEnv era)

instance EraPParams era => NFData (CertEnv era)

data DijkstraCertPredFailure era
  = DelegFailure (PredicateFailure (EraRule "DELEG" era))
  | PoolFailure (PredicateFailure (EraRule "POOL" era))
  | GovCertFailure (PredicateFailure (EraRule "GOVCERT" era))
  deriving (Generic)

type instance EraRuleFailure "CERT" DijkstraEra = DijkstraCertPredFailure DijkstraEra

type instance EraRuleEvent "CERT" DijkstraEra = DijkstraCertEvent DijkstraEra

instance InjectRuleFailure "CERT" DijkstraCertPredFailure DijkstraEra

instance InjectRuleFailure "CERT" DijkstraDelegPredFailure DijkstraEra where
  injectFailure = DelegFailure

instance InjectRuleFailure "CERT" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = PoolFailure

instance InjectRuleFailure "CERT" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = GovCertFailure

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELEG" era))
  , Show (PredicateFailure (EraRule "POOL" era))
  , Show (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  Show (DijkstraCertPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELEG" era))
  , Eq (PredicateFailure (EraRule "POOL" era))
  , Eq (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  Eq (DijkstraCertPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "DELEG" era))
  , NoThunks (PredicateFailure (EraRule "POOL" era))
  , NoThunks (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  NoThunks (DijkstraCertPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "DELEG" era))
  , NFData (PredicateFailure (EraRule "POOL" era))
  , NFData (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  NFData (DijkstraCertPredFailure era)

data DijkstraCertEvent era
  = DelegEvent (Event (EraRule "DELEG" era))
  | PoolEvent (Event (EraRule "POOL" era))
  | GovCertEvent (Event (EraRule "GOVCERT" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "DELEG" era))
  , Eq (Event (EraRule "GOVCERT" era))
  , Eq (Event (EraRule "POOL" era))
  ) =>
  Eq (DijkstraCertEvent era)

instance
  ( NFData (Event (EraRule "DELEG" era))
  , NFData (Event (EraRule "GOVCERT" era))
  , NFData (Event (EraRule "POOL" era))
  ) =>
  NFData (DijkstraCertEvent era)

instance
  forall era.
  ( Era era
  , State (EraRule "DELEG" era) ~ CertState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Environment (EraRule "DELEG" era) ~ DijkstraDelegEnv era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "GOVCERT" era) ~ DijkstraGovCertEnv era
  , Signal (EraRule "DELEG" era) ~ DijkstraDelegCert
  , Signal (EraRule "POOL" era) ~ PoolCert
  , Signal (EraRule "GOVCERT" era) ~ DijkstraGovCert
  , Embed (EraRule "DELEG" era) (DijkstraCERT era)
  , Embed (EraRule "POOL" era) (DijkstraCERT era)
  , Embed (EraRule "GOVCERT" era) (DijkstraCERT era)
  , TxCert era ~ DijkstraTxCert era
  , EraCertState era
  ) =>
  STS (DijkstraCERT era)
  where
  type State (DijkstraCERT era) = CertState era
  type Signal (DijkstraCERT era) = TxCert era
  type Environment (DijkstraCERT era) = CertEnv era
  type BaseM (DijkstraCERT era) = ShelleyBase
  type PredicateFailure (DijkstraCERT era) = DijkstraCertPredFailure era
  type Event (DijkstraCERT era) = DijkstraCertEvent era

  transitionRules = [certTransition @era]

certTransition ::
  forall era.
  ( State (EraRule "DELEG" era) ~ CertState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Environment (EraRule "DELEG" era) ~ DijkstraDelegEnv era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "GOVCERT" era) ~ DijkstraGovCertEnv era
  , Signal (EraRule "DELEG" era) ~ DijkstraDelegCert
  , Signal (EraRule "POOL" era) ~ PoolCert
  , Signal (EraRule "GOVCERT" era) ~ DijkstraGovCert
  , Embed (EraRule "DELEG" era) (DijkstraCERT era)
  , Embed (EraRule "POOL" era) (DijkstraCERT era)
  , Embed (EraRule "GOVCERT" era) (DijkstraCERT era)
  , TxCert era ~ DijkstraTxCert era
  , EraCertState era
  ) =>
  TransitionRule (DijkstraCERT era)
certTransition = do
  TRC (CertEnv pp currentEpoch committee committeeProposals, certState, c) <- judgmentContext
  let
    certPState = certState ^. certPStateL
    pools = psStakePoolParams certPState
  case c of
    DijkstraTxCertDeleg delegCert -> do
      trans @(EraRule "DELEG" era) $ TRC (DijkstraDelegEnv pp pools, certState, delegCert)
    DijkstraTxCertPool poolCert -> do
      newPState <- trans @(EraRule "POOL" era) $ TRC (PoolEnv currentEpoch pp, certPState, poolCert)
      pure $ certState & certPStateL .~ newPState
    DijkstraTxCertGov govCert -> do
      trans @(EraRule "GOVCERT" era) $
        TRC (DijkstraGovCertEnv pp currentEpoch committee committeeProposals, certState, govCert)

instance
  ( Era era
  , STS (DijkstraDELEG era)
  , PredicateFailure (EraRule "DELEG" era) ~ DijkstraDelegPredFailure era
  ) =>
  Embed (DijkstraDELEG era) (DijkstraCERT era)
  where
  wrapFailed = DelegFailure
  wrapEvent = absurd

instance
  ( Era era
  , STS (ShelleyPOOL era)
  , Event (EraRule "POOL" era) ~ PoolEvent era
  , PredicateFailure (EraRule "POOL" era) ~ ShelleyPoolPredFailure era
  , PredicateFailure (ShelleyPOOL era) ~ ShelleyPoolPredFailure era
  , BaseM (ShelleyPOOL era) ~ ShelleyBase
  ) =>
  Embed (ShelleyPOOL era) (DijkstraCERT era)
  where
  wrapFailed = PoolFailure
  wrapEvent = PoolEvent

instance
  ( Era era
  , STS (DijkstraGOVCERT era)
  , PredicateFailure (EraRule "GOVCERT" era) ~ DijkstraGovCertPredFailure era
  ) =>
  Embed (DijkstraGOVCERT era) (DijkstraCERT era)
  where
  wrapFailed = GovCertFailure
  wrapEvent = absurd

instance
  ( Typeable era
  , EncCBOR (PredicateFailure (EraRule "DELEG" era))
  , EncCBOR (PredicateFailure (EraRule "POOL" era))
  , EncCBOR (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  EncCBOR (DijkstraCertPredFailure era)
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
  DecCBOR (DijkstraCertPredFailure era)
  where
  decCBOR =
    decode $ Summands "DijkstraCertPredFailure" $ \case
      1 -> SumD DelegFailure <! From
      2 -> SumD PoolFailure <! From
      3 -> SumD GovCertFailure <! From
      n -> Invalid n
