{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.HardFork (
  ConwayHARDFORK,
  ConwayHardForkEvent (..),
) where

import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayHARDFORK)
import Cardano.Ledger.Shelley.LedgerState
import Control.DeepSeq (NFData)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS (..),
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  tellEvent,
  transitionRules,
 )
import Data.Void (Void)
import GHC.Generics (Generic)

newtype ConwayHardForkEvent era = ConwayHardForkEvent ProtVer
  deriving (Generic, Eq)
  deriving newtype (NFData)

type instance EraRuleEvent "HARDFORK" (ConwayEra c) = ConwayHardForkEvent (ConwayEra c)

instance
  EraGov era =>
  STS (ConwayHARDFORK era)
  where
  type State (ConwayHARDFORK era) = EpochState era
  type Signal (ConwayHARDFORK era) = ProtVer
  type Environment (ConwayHARDFORK era) = ()
  type BaseM (ConwayHARDFORK era) = ShelleyBase
  type PredicateFailure (ConwayHARDFORK era) = Void
  type Event (ConwayHARDFORK era) = ConwayHardForkEvent era

  transitionRules = [hardforkTransition @era]

hardforkTransition :: TransitionRule (ConwayHARDFORK era)
hardforkTransition = do
  TRC (_, st, pv) <-
    judgmentContext
  -- Add operations that modify the state meaningfully during intra-era hardforks.
  tellEvent $ ConwayHardForkEvent pv
  pure st
