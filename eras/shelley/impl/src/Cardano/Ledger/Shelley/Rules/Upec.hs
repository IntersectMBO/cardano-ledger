{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
  ShelleyUPEC,
  UpecState (..),
  ShelleyUpecPredFailure (..),
  votedValue,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyUPEC)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState,
  lsCertState,
  lsUTxOState,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.Rules.Newpp (
  NewppEnv (..),
  ShelleyNEWPP,
  ShelleyNewppState (..),
 )
import Cardano.Ledger.Shelley.Rules.Ppup (votedFuturePParams)
import Control.DeepSeq (NFData)
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  judgmentContext,
  trans,
 )
import Data.Default.Class (Default)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data UpecState era = UpecState
  { usCurPParams :: !(PParams era)
  -- ^ Current protocol parameters.
  , usGovState :: !(ShelleyGovState era)
  -- ^ State of the protocol update transition system.
  }

deriving stock instance
  (Show (PParams era), Show (PParamsUpdate era)) =>
  Show (UpecState era)

newtype ShelleyUpecPredFailure era
  = NewPpFailure (PredicateFailure (ShelleyNEWPP era))
  deriving (Eq, Show, Generic)

instance NoThunks (ShelleyUpecPredFailure era)

instance NFData (ShelleyUpecPredFailure era)

instance
  ( EraGov era
  , Default (PParams era)
  , GovState era ~ ShelleyGovState era
  , ProtVerAtMost era 8
  ) =>
  STS (ShelleyUPEC era)
  where
  type State (ShelleyUPEC era) = UpecState era
  type Signal (ShelleyUPEC era) = ()
  type Environment (ShelleyUPEC era) = LedgerState era
  type BaseM (ShelleyUPEC era) = ShelleyBase
  type PredicateFailure (ShelleyUPEC era) = ShelleyUpecPredFailure era
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
          trans @(ShelleyNEWPP era) $
            TRC (NewppEnv (lsCertState ls) utxoState, NewppState pp ppupState, ppNew)
        pure $! UpecState pp' ppupState'
    ]

instance
  (Era era, STS (ShelleyNEWPP era)) =>
  Embed (ShelleyNEWPP era) (ShelleyUPEC era)
  where
  wrapFailed = NewPpFailure

votedValue ::
  forall era.
  EraPParams era =>
  ProposedPPUpdates era ->
  PParams era ->
  Int ->
  Maybe (PParams era)
votedValue ppups pp = votedFuturePParams ppups pp . fromIntegral
{-# DEPRECATED votedValue "In favor of `votedFuturePParams`" #-}
