{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
  decodeRecordSum,
  encodeListLen,
  encodeWord,
  invalidKey,
 )
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

instance NFData (DijkstraGovCertPredFailure era)

instance Era era => EncCBOR (DijkstraGovCertPredFailure era) where
  encCBOR =
    \case
      DijkstraDRepAlreadyRegistered cred -> encodeListLen 2 <> encodeWord 0 <> encCBOR cred
      DijkstraDRepNotRegistered cred -> encodeListLen 2 <> encodeWord 1 <> encCBOR cred
      DijkstraDRepIncorrectDeposit mm -> encodeListLen 2 <> encodeWord 2 <> encCBOR mm
      DijkstraCommitteeHasPreviouslyResigned coldCred -> encodeListLen 2 <> encodeWord 3 <> encCBOR coldCred
      DijkstraDRepIncorrectRefund mm -> encodeListLen 2 <> encodeWord 4 <> encCBOR mm
      DijkstraCommitteeIsUnknown coldCred -> encodeListLen 2 <> encodeWord 5 <> encCBOR coldCred

instance Typeable era => DecCBOR (DijkstraGovCertPredFailure era) where
  decCBOR = decodeRecordSum "DijkstraGovCertPredFailure" $ \case
    0 -> fmap (2,) $ DijkstraDRepAlreadyRegistered <$> decCBOR
    1 -> fmap (2,) $ DijkstraDRepNotRegistered <$> decCBOR
    2 -> fmap (2,) $ DijkstraDRepIncorrectDeposit <$> decCBOR
    3 -> fmap (2,) $ DijkstraCommitteeHasPreviouslyResigned <$> decCBOR
    4 -> fmap (2,) $ DijkstraDRepIncorrectRefund <$> decCBOR
    5 -> fmap (2,) $ DijkstraCommitteeIsUnknown <$> decCBOR
    n -> invalidKey n

instance
  ( ConwayEraPParams era
  , ConwayEraCertState era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Signal (EraRule "GOVCERT" era) ~ ConwayGovCert
  , Environment (EraRule "GOVCERT" era) ~ ConwayGovCertEnv era
  , InjectRuleFailure "GOVCERT" ConwayGovCertPredFailure era
  , EraRule "GOVCERT" era ~ DijkstraGOVCERT era
  , Eq (PredicateFailure (EraRule "GOVCERT" era))
  , Show (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  STS (DijkstraGOVCERT era)
  where
  type State (DijkstraGOVCERT era) = CertState era
  type Signal (DijkstraGOVCERT era) = ConwayGovCert
  type Environment (DijkstraGOVCERT era) = ConwayGovCertEnv era
  type BaseM (DijkstraGOVCERT era) = ShelleyBase
  type PredicateFailure (DijkstraGOVCERT era) = DijkstraGovCertPredFailure era
  type Event (DijkstraGOVCERT era) = Void

  transitionRules = [Conway.conwayGovCertTransition]

conwayToDijkstraGovCertPredFailure ::
  forall era. ConwayGovCertPredFailure era -> DijkstraGovCertPredFailure era
conwayToDijkstraGovCertPredFailure = \case
  ConwayDRepAlreadyRegistered c -> DijkstraDRepAlreadyRegistered c
  ConwayDRepNotRegistered c -> DijkstraDRepNotRegistered c
  ConwayDRepIncorrectDeposit mm -> DijkstraDRepIncorrectDeposit mm
  ConwayCommitteeHasPreviouslyResigned c -> DijkstraCommitteeHasPreviouslyResigned c
  ConwayDRepIncorrectRefund mm -> DijkstraDRepIncorrectRefund mm
  ConwayCommitteeIsUnknown c -> DijkstraCommitteeIsUnknown c
