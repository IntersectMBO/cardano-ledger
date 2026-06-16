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
  SUBCERT,
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
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  SUBCERT,
  SUBDELEG,
  SUBGOVCERT,
  SUBPOOL,
 )
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubDeleg (DijkstraSubDelegPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubGovCert (DijkstraSubGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubPool (DijkstraSubPoolEvent, DijkstraSubPoolPredFailure)
import Cardano.Ledger.Dijkstra.TxCert
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.Void (absurd)
import GHC.Generics (Generic)
import Lens.Micro

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

instance InjectRuleFailure "SUBCERT" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraSubCertPredFailure

instance InjectRuleEvent "SUBCERT" DijkstraSubCertEvent DijkstraEra

newtype DijkstraSubCertEvent era = SubPoolEvent (Event (EraRule "SUBPOOL" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBPOOL" era)) => Eq (DijkstraSubCertEvent era)

instance NFData (Event (EraRule "SUBPOOL" era)) => NFData (DijkstraSubCertEvent era)

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERT" era ~ SUBCERT era
  , EraRule "SUBDELEG" era ~ SUBDELEG era
  , EraRule "SUBPOOL" era ~ SUBPOOL era
  , EraRule "SUBGOVCERT" era ~ SUBGOVCERT era
  , Embed (EraRule "SUBDELEG" era) (SUBCERT era)
  , Embed (EraRule "SUBPOOL" era) (SUBCERT era)
  , Embed (EraRule "SUBGOVCERT" era) (SUBCERT era)
  , TxCert era ~ DijkstraTxCert era
  ) =>
  STS (SUBCERT era)
  where
  type State (SUBCERT era) = CertState era
  type Signal (SUBCERT era) = TxCert era
  type Environment (SUBCERT era) = Conway.CertEnv era
  type BaseM (SUBCERT era) = ShelleyBase
  type PredicateFailure (SUBCERT era) = DijkstraSubCertPredFailure era
  type Event (SUBCERT era) = DijkstraSubCertEvent era

  transitionRules = [dijkstraSubCertTransition @era]

dijkstraSubCertTransition ::
  forall era.
  ( ConwayEraCertState era
  , EraRule "SUBCERT" era ~ SUBCERT era
  , EraRule "SUBDELEG" era ~ SUBDELEG era
  , EraRule "SUBPOOL" era ~ SUBPOOL era
  , EraRule "SUBGOVCERT" era ~ SUBGOVCERT era
  , Embed (EraRule "SUBDELEG" era) (SUBCERT era)
  , Embed (EraRule "SUBPOOL" era) (SUBCERT era)
  , Embed (EraRule "SUBGOVCERT" era) (SUBCERT era)
  , TxCert era ~ DijkstraTxCert era
  ) =>
  TransitionRule (EraRule "SUBCERT" era)
dijkstraSubCertTransition = do
  TRC (Conway.CertEnv pp currentEpoch committee committeeProposals, certState, c) <- judgmentContext
  let
    certPState = certState ^. certPStateL
    pools = psStakePools certPState
  case c of
    DijkstraTxCertDeleg delegCert ->
      trans @(EraRule "SUBDELEG" era) $
        TRC (Conway.ConwayDelegEnv pp pools, certState, dijkstraToConwayDelegCert delegCert)
    DijkstraTxCertPool poolCert -> do
      newPState <-
        trans @(EraRule "SUBPOOL" era) $ TRC (Shelley.PoolEnv currentEpoch pp, certPState, poolCert)
      pure $ certState & certPStateL .~ newPState
    DijkstraTxCertGov govCert -> do
      trans @(EraRule "SUBGOVCERT" era) $
        TRC (Conway.ConwayGovCertEnv pp currentEpoch committee committeeProposals, certState, govCert)

instance
  ( STS (SUBDELEG era)
  , PredicateFailure (EraRule "SUBDELEG" era) ~ DijkstraSubDelegPredFailure era
  ) =>
  Embed (SUBDELEG era) (SUBCERT era)
  where
  wrapFailed = SubDelegFailure
  wrapEvent = absurd

instance
  ( STS (SUBPOOL era)
  , PredicateFailure (EraRule "SUBPOOL" era) ~ DijkstraSubPoolPredFailure era
  , Event (EraRule "SUBPOOL" era) ~ DijkstraSubPoolEvent era
  ) =>
  Embed (SUBPOOL era) (SUBCERT era)
  where
  wrapFailed = SubPoolFailure
  wrapEvent = SubPoolEvent

instance
  ( Era era
  , STS (SUBGOVCERT era)
  , PredicateFailure (EraRule "SUBGOVCERT" era) ~ DijkstraSubGovCertPredFailure era
  ) =>
  Embed (SUBGOVCERT era) (SUBCERT era)
  where
  wrapFailed = SubGovCertFailure
  wrapEvent = absurd

conwayToDijkstraSubCertPredFailure ::
  forall era.
  ( InjectRuleFailure "SUBDELEG" Conway.ConwayDelegPredFailure era
  , PredicateFailure (EraRule "DELEG" era) ~ Conway.ConwayDelegPredFailure era
  , InjectRuleFailure "SUBPOOL" Shelley.ShelleyPoolPredFailure era
  , PredicateFailure (EraRule "POOL" era) ~ Shelley.ShelleyPoolPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraGovCertPredFailure era
  , PredicateFailure (EraRule "GOVCERT" era) ~ DijkstraGovCertPredFailure era
  ) =>
  Conway.ConwayCertPredFailure era -> DijkstraSubCertPredFailure era
conwayToDijkstraSubCertPredFailure = \case
  Conway.DelegFailure f -> SubDelegFailure (injectFailure @"SUBDELEG" f)
  Conway.PoolFailure f -> SubPoolFailure (injectFailure @"SUBPOOL" f)
  Conway.GovCertFailure f -> SubGovCertFailure (injectFailure @"SUBGOVCERT" f)
