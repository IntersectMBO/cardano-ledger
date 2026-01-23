{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Cardano.Ledger.Dijkstra.Rules.SubCert (
  DijkstraSUBCERT,
  DijkstraSubCertPredFailure (..),
  DijkstraSubCertEvent (..),
) where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (ConwayEraGov)
import Cardano.Ledger.Conway.Rules (
  CertEnv (..),
  ConwayDelegEnv (..),
  ConwayGovCertEnv (..),
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBCERT,
  DijkstraSUBDELEG,
  DijkstraSUBGOVCERT,
  DijkstraSUBPOOL,
 )
import Cardano.Ledger.Dijkstra.Rules.SubDeleg (DijkstraSubDelegPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubGovCert (DijkstraSubGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubPool (DijkstraSubPoolEvent, DijkstraSubPoolPredFailure)
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Shelley.Rules (PoolEnv (..), PoolEvent, ShelleyPoolPredFailure)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.Void (absurd)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data DijkstraSubCertPredFailure era
  = SubDelegFailure (PredicateFailure (EraRule "SUBDELEG" era))
  | SubPoolFailure (PredicateFailure (EraRule "SUBPOOL" era))
  | SubGovCertFailure (PredicateFailure (EraRule "SUBGOVCERT" era))
  deriving (Generic)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "SUBDELEG" era))
  , Eq (PredicateFailure (EraRule "SUBPOOL" era))
  , Eq (PredicateFailure (EraRule "SUBGOVCERT" era))
  ) =>
  Eq (DijkstraSubCertPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "SUBDELEG" era))
  , Show (PredicateFailure (EraRule "SUBPOOL" era))
  , Show (PredicateFailure (EraRule "SUBGOVCERT" era))
  ) =>
  Show (DijkstraSubCertPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "SUBDELEG" era))
  , NoThunks (PredicateFailure (EraRule "SUBPOOL" era))
  , NoThunks (PredicateFailure (EraRule "SUBGOVCERT" era))
  ) =>
  NoThunks (DijkstraSubCertPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "SUBDELEG" era))
  , NFData (PredicateFailure (EraRule "SUBPOOL" era))
  , NFData (PredicateFailure (EraRule "SUBGOVCERT" era))
  ) =>
  NFData (DijkstraSubCertPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBDELEG" era))
  , EncCBOR (PredicateFailure (EraRule "SUBPOOL" era))
  , EncCBOR (PredicateFailure (EraRule "SUBGOVCERT" era))
  ) =>
  EncCBOR (DijkstraSubCertPredFailure era)
  where
  encCBOR =
    encode . \case
      SubDelegFailure x -> Sum (SubDelegFailure @era) 1 !> To x
      SubPoolFailure x -> Sum (SubPoolFailure @era) 2 !> To x
      SubGovCertFailure x -> Sum (SubGovCertFailure @era) 3 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBDELEG" era))
  , DecCBOR (PredicateFailure (EraRule "SUBPOOL" era))
  , DecCBOR (PredicateFailure (EraRule "SUBGOVCERT" era))
  ) =>
  DecCBOR (DijkstraSubCertPredFailure era)
  where
  decCBOR =
    decode $ Summands "DijkstraSubCertPredFailure" $ \case
      1 -> SumD SubDelegFailure <! From
      2 -> SumD SubPoolFailure <! From
      3 -> SumD SubGovCertFailure <! From
      n -> Invalid n

type instance EraRuleFailure "SUBCERT" DijkstraEra = DijkstraSubCertPredFailure DijkstraEra

type instance EraRuleEvent "SUBCERT" DijkstraEra = DijkstraSubCertEvent DijkstraEra

instance InjectRuleFailure "SUBCERT" DijkstraSubCertPredFailure DijkstraEra

instance InjectRuleFailure "SUBCERT" DijkstraSubDelegPredFailure DijkstraEra where
  injectFailure = SubDelegFailure

instance InjectRuleFailure "SUBCERT" DijkstraSubPoolPredFailure DijkstraEra where
  injectFailure = SubPoolFailure

instance InjectRuleFailure "SUBCERT" DijkstraSubGovCertPredFailure DijkstraEra where
  injectFailure = SubGovCertFailure

instance InjectRuleEvent "SUBCERT" DijkstraSubCertEvent DijkstraEra

newtype DijkstraSubCertEvent era = SubPoolEvent (Event (EraRule "SUBPOOL" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBPOOL" era)) => Eq (DijkstraSubCertEvent era)

instance NFData (Event (EraRule "SUBPOOL" era)) => NFData (DijkstraSubCertEvent era)

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , Embed (EraRule "SUBDELEG" era) (DijkstraSUBCERT era)
  , Embed (EraRule "SUBPOOL" era) (DijkstraSUBCERT era)
  , Embed (EraRule "SUBGOVCERT" era) (DijkstraSUBCERT era)
  , TxCert era ~ DijkstraTxCert era
  ) =>
  STS (DijkstraSUBCERT era)
  where
  type State (DijkstraSUBCERT era) = CertState era
  type Signal (DijkstraSUBCERT era) = TxCert era
  type Environment (DijkstraSUBCERT era) = CertEnv era
  type BaseM (DijkstraSUBCERT era) = ShelleyBase
  type PredicateFailure (DijkstraSUBCERT era) = DijkstraSubCertPredFailure era
  type Event (DijkstraSUBCERT era) = DijkstraSubCertEvent era

  transitionRules = [dijkstraSubCertTransition @era]

dijkstraSubCertTransition ::
  forall era.
  ( ConwayEraCertState era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , Embed (EraRule "SUBDELEG" era) (DijkstraSUBCERT era)
  , Embed (EraRule "SUBPOOL" era) (DijkstraSUBCERT era)
  , Embed (EraRule "SUBGOVCERT" era) (DijkstraSUBCERT era)
  , TxCert era ~ DijkstraTxCert era
  ) =>
  TransitionRule (EraRule "SUBCERT" era)
dijkstraSubCertTransition = do
  TRC (CertEnv pp currentEpoch committee committeeProposals, certState, c) <- judgmentContext
  let
    certPState = certState ^. certPStateL
    pools = psStakePools certPState
  case c of
    DijkstraTxCertDeleg delegCert ->
      trans @(EraRule "SUBDELEG" era) $
        TRC (ConwayDelegEnv pp pools, certState, delegCert)
    DijkstraTxCertPool poolCert -> do
      newPState <- trans @(EraRule "SUBPOOL" era) $ TRC (PoolEnv currentEpoch pp, certPState, poolCert)
      pure $ certState & certPStateL .~ newPState
    DijkstraTxCertGov govCert -> do
      trans @(EraRule "SUBGOVCERT" era) $
        TRC (ConwayGovCertEnv pp currentEpoch committee committeeProposals, certState, govCert)

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  ) =>
  Embed (DijkstraSUBDELEG era) (DijkstraSUBCERT era)
  where
  wrapFailed = SubDelegFailure
  wrapEvent = absurd

instance
  ( ConwayEraGov era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleEvent "SUBPOOL" PoolEvent era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" ShelleyPoolPredFailure era
  ) =>
  Embed (DijkstraSUBPOOL era) (DijkstraSUBCERT era)
  where
  wrapFailed = SubPoolFailure
  wrapEvent = SubPoolEvent

instance
  ( ConwayEraGov era
  , ConwayEraPParams era
  , ConwayEraCertState era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , InjectRuleFailure "SUBGOVCERT" ConwayGovCertPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure era
  ) =>
  Embed (DijkstraSUBGOVCERT era) (DijkstraSUBCERT era)
  where
  wrapFailed = SubGovCertFailure
  wrapEvent = absurd
