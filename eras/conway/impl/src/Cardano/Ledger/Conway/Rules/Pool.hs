{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Pool (
  ConwayPOOL,
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.CertState (PState (..))
import Cardano.Ledger.Conway.Era (ConwayPOOL)
import Cardano.Ledger.Core (
  EraCrypto,
  EraPParams,
  EraRule,
  PoolCert,
 )
import Cardano.Ledger.Shelley.API (
  PoolEnv,
 )
import Cardano.Ledger.Shelley.Rules (
  PoolEvent,
  ShelleyPoolPredFailure,
  poolCertTransition,
 )
import Cardano.Ledger.Shelley.TxCert (ShelleyEraTxCert)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS (..),
  Signal,
  State,
  transitionRules,
 )
import Control.State.Transition.Extended (TRC (TRC), TransitionRule, judgmentContext)

instance
  forall era.
  ( EraPParams era
  , State (EraRule "POOL" era) ~ PState era
  , Signal (EraRule "POOL" era) ~ PoolCert (EraCrypto era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , EraRule "POOL" era ~ ConwayPOOL era
  , ShelleyEraTxCert era
  ) =>
  STS (ConwayPOOL era)
  where
  type State (ConwayPOOL era) = PState era
  type Signal (ConwayPOOL era) = PoolCert (EraCrypto era)
  type Environment (ConwayPOOL era) = PoolEnv era
  type BaseM (ConwayPOOL era) = ShelleyBase
  type PredicateFailure (ConwayPOOL era) = ShelleyPoolPredFailure era
  type Event (ConwayPOOL era) = PoolEvent era

  transitionRules = [conwayPoolTransition]

conwayPoolTransition ::
  ( EraPParams era
  , ShelleyEraTxCert era
  , EraRule "POOL" era ~ ConwayPOOL era
  ) =>
  TransitionRule (ConwayPOOL era)
conwayPoolTransition = do
  TRC (penv, pState, c) <- judgmentContext
  poolCertTransition penv pState c
