{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Epoch change registration.
--
-- The rules of this module determine how the update subsystem of the ledger
-- handles the epoch transitions.
module Cardano.Ledger.Shelley.Rules.Upec (
  UPEC,
  UpecState (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (UPEC)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState,
  lsCertState,
  lsUTxOState,
 )
import Cardano.Ledger.Shelley.Rules.Newpp (
  NEWPP,
  NewppEnv (..),
  ShelleyNewppState (..),
 )
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  judgmentContext,
  trans,
 )
import Data.Default (Default)
import Data.Void (Void)

data UpecState era = UpecState
  { usCurPParams :: !(PParams era)
  -- ^ Current protocol parameters.
  , usGovState :: !(ShelleyGovState era)
  -- ^ State of the protocol update transition system.
  }

deriving stock instance
  (Show (PParams era), Show (PParamsUpdate era)) =>
  Show (UpecState era)

instance
  ( EraGov era
  , Default (PParams era)
  , GovState era ~ ShelleyGovState era
  , AtMostEra "Babbage" era
  ) =>
  STS (UPEC era)
  where
  type State (UPEC era) = UpecState era
  type Signal (UPEC era) = ()
  type Environment (UPEC era) = LedgerState era
  type BaseM (UPEC era) = ShelleyBase
  type PredicateFailure (UPEC era) = Void
  initialRules = []
  transitionRules =
    [ do
        TRC
          ( ls
            , UpecState pp ppupState
            , _
            ) <-
          judgmentContext

        let utxoState = lsUTxOState ls
            ppNew = nextEpochPParams ppupState
        NewppState pp' ppupState' <-
          trans @(NEWPP era) $
            TRC (NewppEnv (lsCertState ls) utxoState, NewppState pp ppupState, ppNew)
        pure $! UpecState pp' ppupState'
    ]

instance
  (Era era, STS (NEWPP era)) =>
  Embed (NEWPP era) (UPEC era)
  where
  wrapFailed = \case {}
