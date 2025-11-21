{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.GovCert (
  DijkstraGOVCERT,
  DijkstraGovCertPredFailure (..),
  conwayToDijkstraGovCertPredFailure,
) where

import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Relation (..),
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayGovCertEnv, ConwayGovCertPredFailure (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraGOVCERT)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  transitionRules,
 )
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraGovCertPredFailure era
  = DijkstraDRepAlreadyRegistered (Credential DRepRole)
  | DijkstraDRepNotRegistered (Credential DRepRole)
  | DijkstraDRepIncorrectDeposit (Mismatch RelEQ Coin)
  | DijkstraCommitteeHasPreviouslyResigned (Credential ColdCommitteeRole)
  | DijkstraDRepIncorrectRefund (Mismatch RelEQ Coin)
  | -- | Predicate failure whenever an update to an unknown committee member is
    -- attempted. Current Constitutional Committee and all available proposals will be
    -- searched before reporting this predicate failure.
    DijkstraCommitteeIsUnknown (Credential ColdCommitteeRole)
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "GOVCERT" DijkstraEra = DijkstraGovCertPredFailure DijkstraEra

type instance EraRuleEvent "GOVCERT" DijkstraEra = VoidEraRule "GOVCERT" DijkstraEra

instance InjectRuleFailure "GOVCERT" DijkstraGovCertPredFailure DijkstraEra

instance InjectRuleFailure "GOVCERT" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraGovCertPredFailure

instance NoThunks (DijkstraGovCertPredFailure era)

instance NFData (DijkstraGovCertPredFailure era)

instance Era era => EncCBOR (DijkstraGovCertPredFailure era) where
  encCBOR =
    encode @_ @(DijkstraGovCertPredFailure era) . \case
      DijkstraDRepAlreadyRegistered cred -> Sum DijkstraDRepAlreadyRegistered 0 !> To cred
      DijkstraDRepNotRegistered cred -> Sum DijkstraDRepNotRegistered 1 !> To cred
      DijkstraDRepIncorrectDeposit mm -> Sum DijkstraDRepIncorrectDeposit 2 !> To mm
      DijkstraCommitteeHasPreviouslyResigned coldCred -> Sum DijkstraCommitteeHasPreviouslyResigned 3 !> To coldCred
      DijkstraDRepIncorrectRefund mm -> Sum DijkstraDRepIncorrectRefund 4 !> To mm
      DijkstraCommitteeIsUnknown coldCred -> Sum DijkstraCommitteeIsUnknown 5 !> To coldCred

instance Typeable era => DecCBOR (DijkstraGovCertPredFailure era) where
  decCBOR = decode . Summands "DijkstraGovCertPredFailure" $ \case
    0 -> SumD DijkstraDRepAlreadyRegistered <! From
    1 -> SumD DijkstraDRepNotRegistered <! From
    2 -> SumD DijkstraDRepIncorrectDeposit <! From
    3 -> SumD DijkstraCommitteeHasPreviouslyResigned <! From
    4 -> SumD DijkstraDRepIncorrectRefund <! From
    5 -> SumD DijkstraCommitteeIsUnknown <! From
    n -> Invalid n

instance
  ( ConwayEraPParams era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Signal (EraRule "GOVCERT" era) ~ ConwayGovCert
  , Environment (EraRule "GOVCERT" era) ~ ConwayGovCertEnv era
  , InjectRuleFailure "GOVCERT" ConwayGovCertPredFailure era
  , EraRule "GOVCERT" era ~ DijkstraGOVCERT era
  , Eq (PredicateFailure (EraRule "GOVCERT" era))
  , Show (PredicateFailure (EraRule "GOVCERT" era))
  , ConwayEraCertState era
  ) =>
  STS (DijkstraGOVCERT era)
  where
  type State (DijkstraGOVCERT era) = CertState era
  type Signal (DijkstraGOVCERT era) = ConwayGovCert
  type Environment (DijkstraGOVCERT era) = ConwayGovCertEnv era
  type BaseM (DijkstraGOVCERT era) = ShelleyBase
  type PredicateFailure (DijkstraGOVCERT era) = DijkstraGovCertPredFailure era
  type Event (DijkstraGOVCERT era) = Void

  transitionRules = [Conway.conwayGovCertTransition @era]

conwayToDijkstraGovCertPredFailure ::
  forall era. ConwayGovCertPredFailure era -> DijkstraGovCertPredFailure era
conwayToDijkstraGovCertPredFailure = \case
  ConwayDRepAlreadyRegistered c -> DijkstraDRepAlreadyRegistered c
  ConwayDRepNotRegistered c -> DijkstraDRepNotRegistered c
  ConwayDRepIncorrectDeposit mm -> DijkstraDRepIncorrectDeposit mm
  ConwayCommitteeHasPreviouslyResigned c -> DijkstraCommitteeHasPreviouslyResigned c
  ConwayDRepIncorrectRefund mm -> DijkstraDRepIncorrectRefund mm
  ConwayCommitteeIsUnknown c -> DijkstraCommitteeIsUnknown c
