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
import Cardano.Ledger.Conway.Rules (CertsEnv)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBCERTS,
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
import Data.Sequence (Seq (..))
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraSubCertsPredFailure era = DijkstraSubCertsPredFailure
  deriving (Show, Eq, Generic)

instance NoThunks (DijkstraSubCertsPredFailure era)

instance NFData (DijkstraSubCertsPredFailure era)

instance Era era => EncCBOR (DijkstraSubCertsPredFailure era) where
  encCBOR _ = encCBOR ()

instance Typeable era => DecCBOR (DijkstraSubCertsPredFailure era) where
  decCBOR = decCBOR @() *> pure DijkstraSubCertsPredFailure

type instance EraRuleFailure "SUBCERTS" DijkstraEra = DijkstraSubCertsPredFailure DijkstraEra

type instance EraRuleEvent "SUBCERTS" DijkstraEra = VoidEraRule "SUBCERTS" DijkstraEra

instance InjectRuleFailure "SUBCERTS" DijkstraSubCertsPredFailure DijkstraEra

instance
  ( EraGov era
  , EraCertState era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
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

dijkstraSubCertsTransition :: TransitionRule (EraRule "SUBCERTS" era)
dijkstraSubCertsTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
