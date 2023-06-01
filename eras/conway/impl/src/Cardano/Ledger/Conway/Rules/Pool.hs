{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Pool (
  ConwayPOOL,
  ConwayPoolEvent (..),
  ConwayPoolPredFailure (..),
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.CertState (PState)
import Cardano.Ledger.Conway.Era (ConwayPOOL)
import Cardano.Ledger.Core (Era (EraCrypto), EraRule)
import Cardano.Ledger.Shelley.API (PoolCert, PoolEnv)
import Control.DeepSeq (NFData)
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data ConwayPoolPredFailure era
  = ConwayPoolPredFailure
  deriving (Show, Eq, Generic, NoThunks, NFData)

newtype ConwayPoolEvent era = PoolEvent (Event (EraRule "POOL" era))

instance
  ( Era era
  , State (EraRule "POOL" era) ~ PState era
  , Signal (EraRule "POOL" era) ~ PoolCert (EraCrypto era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , EraRule "POOL" era ~ ConwayPOOL era
  ) =>
  STS (ConwayPOOL era)
  where
  type State (ConwayPOOL era) = PState era
  type Signal (ConwayPOOL era) = PoolCert (EraCrypto era)
  type Environment (ConwayPOOL era) = PoolEnv era
  type BaseM (ConwayPOOL era) = ShelleyBase
  type PredicateFailure (ConwayPOOL era) = ConwayPoolPredFailure era
  type Event (ConwayPOOL era) = ConwayPoolEvent era

  transitionRules = undefined -- TODO
