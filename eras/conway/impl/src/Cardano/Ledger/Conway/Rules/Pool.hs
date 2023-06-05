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
  EraPParams,
  EraRule,
  EraTxCert (TxCert),
 )
import Cardano.Ledger.Shelley.API (
  PoolEnv,
 )
import Cardano.Ledger.Shelley.Rules (
  PoolEvent,
  ShelleyPoolPredFailure,
  poolDelegationTransition,
 )
import Cardano.Ledger.Shelley.TxCert (ShelleyEraTxCert)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  transitionRules,
 )

instance
  ( EraPParams era
  , State (EraRule "POOL" era) ~ PState era
  , Signal (EraRule "POOL" era) ~ TxCert era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , EraRule "POOL" era ~ ConwayPOOL era
  , ShelleyEraTxCert era
  ) =>
  STS (ConwayPOOL era)
  where
  type State (ConwayPOOL era) = PState era
  type Signal (ConwayPOOL era) = TxCert era
  type Environment (ConwayPOOL era) = PoolEnv era
  type BaseM (ConwayPOOL era) = ShelleyBase
  type PredicateFailure (ConwayPOOL era) = ShelleyPoolPredFailure era
  type Event (ConwayPOOL era) = PoolEvent era

  transitionRules = [poolDelegationTransition]
