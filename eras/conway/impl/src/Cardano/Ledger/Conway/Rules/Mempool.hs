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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Mempool (
  ConwayMEMPOOL,
  ConwayMempoolPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), FromCBOR, ToCBOR)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayMEMPOOL)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
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
  transitionRules,
 )
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

newtype ConwayMempoolPredFailure era = ConwayMempoolPredFailure Text
  deriving (Eq, Show, Generic)
  deriving newtype (NoThunks, NFData, ToCBOR, FromCBOR, EncCBOR, DecCBOR)

type instance EraRuleFailure "MEMPOOL" (ConwayEra c) = ConwayMempoolPredFailure (ConwayEra c)
instance InjectRuleFailure "MEMPOOL" ConwayMempoolPredFailure (ConwayEra c)

type instance EraRuleEvent "MEMPOOL" (ConwayEra c) = VoidEraRule "MEMPOOL" (ConwayEra c)

instance
  ( EraGov era
  , State (EraRule "MEMPOOL" era) ~ LedgerState era
  , Signal (EraRule "MEMPOOL" era) ~ Tx era
  , Environment (EraRule "MEMPOOL" era) ~ LedgerEnv era
  , EraRule "MEMPOOL" era ~ ConwayMEMPOOL era
  ) =>
  STS (ConwayMEMPOOL era)
  where
  type State (ConwayMEMPOOL era) = LedgerState era
  type Signal (ConwayMEMPOOL era) = Tx era
  type Environment (ConwayMEMPOOL era) = LedgerEnv era
  type BaseM (ConwayMEMPOOL era) = ShelleyBase
  type PredicateFailure (ConwayMEMPOOL era) = ConwayMempoolPredFailure era
  type Event (ConwayMEMPOOL era) = Void

  transitionRules = [mempoolTransition @era]

mempoolTransition :: TransitionRule (ConwayMEMPOOL era)
mempoolTransition = do
  TRC (_ledgerEnv, _ledgerState, _tx) <-
    judgmentContext
  -- This rule only gets invoked on transactions within the mempool.
  -- Add checks here that sanitize undesired transactions.
  pure _ledgerState
