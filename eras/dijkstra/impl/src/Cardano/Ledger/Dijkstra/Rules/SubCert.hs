{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubCert (
  DijkstraSUBCERT,
  DijkstraSubCertPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (CertEnv)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBCERT,
 )
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  transitionRules,
 )
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraSubCertPredFailure era = DijkstraSubCertPredFailure
  deriving (Show, Eq, Generic)

instance NoThunks (DijkstraSubCertPredFailure era)

instance NFData (DijkstraSubCertPredFailure era)

instance Era era => EncCBOR (DijkstraSubCertPredFailure era) where
  encCBOR _ = encCBOR ()

instance Typeable era => DecCBOR (DijkstraSubCertPredFailure era) where
  decCBOR = decCBOR @() *> pure DijkstraSubCertPredFailure

type instance EraRuleFailure "SUBCERT" DijkstraEra = DijkstraSubCertPredFailure DijkstraEra

type instance EraRuleEvent "SUBCERT" DijkstraEra = VoidEraRule "SUBCERT" DijkstraEra

instance InjectRuleFailure "SUBCERT" DijkstraSubCertPredFailure DijkstraEra

instance
  ( EraGov era
  , EraCertState era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  ) =>
  STS (DijkstraSUBCERT era)
  where
  type State (DijkstraSUBCERT era) = CertState era
  type Signal (DijkstraSUBCERT era) = TxCert era
  type Environment (DijkstraSUBCERT era) = CertEnv era
  type BaseM (DijkstraSUBCERT era) = ShelleyBase
  type PredicateFailure (DijkstraSUBCERT era) = DijkstraSubCertPredFailure era
  type Event (DijkstraSUBCERT era) = Void

  transitionRules = [dijkstraSubCertTransition @era]

dijkstraSubCertTransition :: TransitionRule (EraRule "SUBCERT" era)
dijkstraSubCertTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
