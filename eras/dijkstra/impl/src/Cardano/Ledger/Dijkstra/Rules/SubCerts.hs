{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubCerts (
  DijkstraSUBCERTS,
  DijkstraSubCertsPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (ConwayEraGov)
import Cardano.Ledger.Conway.Rules (CertEnv (..), CertsEnv (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBCERT,
  DijkstraSUBCERTS,
  DijkstraSUBDELEG,
  DijkstraSUBGOVCERT,
  DijkstraSUBPOOL,
 )
import Cardano.Ledger.Dijkstra.Rules.SubCert (DijkstraSubCertPredFailure)
import Cardano.Ledger.Dijkstra.TxCert
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.Sequence (Seq (..))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraSubCertsPredFailure era
  = SubCertFailure (PredicateFailure (EraRule "SUBCERT" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBCERT" era)) => Eq (DijkstraSubCertsPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBCERT" era)) => Show (DijkstraSubCertsPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "SUBCERT" era)) =>
  NoThunks (DijkstraSubCertsPredFailure era)

instance
  NFData (PredicateFailure (EraRule "SUBCERT" era)) =>
  NFData (DijkstraSubCertsPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBCERT" era))
  ) =>
  EncCBOR (DijkstraSubCertsPredFailure era)
  where
  encCBOR (SubCertFailure e) = encCBOR e

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBCERT" era))
  ) =>
  DecCBOR (DijkstraSubCertsPredFailure era)
  where
  decCBOR = SubCertFailure <$> decCBOR

type instance EraRuleFailure "SUBCERTS" DijkstraEra = DijkstraSubCertsPredFailure DijkstraEra

type instance EraRuleEvent "SUBCERTS" DijkstraEra = VoidEraRule "SUBCERTS" DijkstraEra

instance InjectRuleFailure "SUBCERTS" DijkstraSubCertsPredFailure DijkstraEra

instance InjectRuleFailure "SUBCERTS" DijkstraSubCertPredFailure DijkstraEra where
  injectFailure = SubCertFailure

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , Embed (EraRule "SUBCERT" era) (DijkstraSUBCERTS era)
  ) =>
  STS (DijkstraSUBCERTS era)
  where
  type State (DijkstraSUBCERTS era) = CertState era
  type Signal (DijkstraSUBCERTS era) = Seq (TxCert era)
  type Environment (DijkstraSUBCERTS era) = CertsEnv era
  type BaseM (DijkstraSUBCERTS era) = ShelleyBase
  type PredicateFailure (DijkstraSUBCERTS era) = DijkstraSubCertsPredFailure era
  type Event (DijkstraSUBCERTS era) = Void

  transitionRules = [dijkstraSubCertsTransition @era]

dijkstraSubCertsTransition ::
  forall era.
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , Embed (EraRule "SUBCERT" era) (DijkstraSUBCERTS era)
  ) =>
  TransitionRule (EraRule "SUBCERTS" era)
dijkstraSubCertsTransition = do
  TRC
    ( env@(CertsEnv _tx pp currentEpoch committee committeeProposals)
      , certState
      , certificates
      ) <-
    judgmentContext
  case certificates of
    Empty -> pure certState
    gamma :|> txCert -> do
      certStateRest <-
        trans @(DijkstraSUBCERTS era) $ TRC (env, certState, gamma)
      trans @(EraRule "SUBCERT" era) $
        TRC (CertEnv pp currentEpoch committee committeeProposals, certStateRest, txCert)

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  Embed (DijkstraSUBCERT era) (DijkstraSUBCERTS era)
  where
  wrapFailed = SubCertFailure
  wrapEvent = absurd
