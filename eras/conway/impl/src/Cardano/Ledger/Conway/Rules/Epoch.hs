{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Epoch (
  ConwayEPOCH,
  PredicateFailure,
  ConwayEpochEvent (..),
  ConwayEpochPredFailure (..),
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEPOCH, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  ConwayGovernance (..),
  ConwayTallyState (..),
  RatifyState (..),
  cgRatifyL,
  cgTallyL,
 )
import Cardano.Ledger.Conway.Rules.Enact (EnactPredFailure)
import Cardano.Ledger.Conway.Rules.Ratify (RatifyEnv (..), RatifySignal (..))
import Cardano.Ledger.EpochBoundary (SnapShots)
import Cardano.Ledger.PoolDistr (PoolDistr)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  IncrementalStake (..),
  PState (..),
  UTxOState (..),
  asReserves,
  esAccountState,
  esLState,
  esLStateL,
  esNonMyopic,
  esPp,
  esPrevPp,
  esSnapshots,
  lsCertState,
  lsUTxOState,
  lsUTxOStateL,
  obligationCertState,
  utxosGovernanceL,
  pattern CertState,
  pattern EpochState,
 )
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyPOOLREAP,
  ShelleyPoolreapEvent,
  ShelleyPoolreapPredFailure,
  ShelleyPoolreapState (..),
  ShelleySNAP,
  ShelleySnapPredFailure,
  SnapEnv (..),
  UpecPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Slot (EpochNo)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import Data.Void (Void, absurd)
import Lens.Micro ((&), (.~))

data ConwayEpochEvent era
  = PoolReapEvent (Event (EraRule "POOLREAP" era))
  | SnapEvent (Event (EraRule "SNAP" era))

data ConwayEpochPredFailure era
  = ConwayRatifyFailure !(PredicateFailure (EraRule "RATIFY" era))
  | ConwayPoolReapFailure !(PredicateFailure (EraRule "POOLREAP" era))
  | ConwaySnapFailure !(PredicateFailure (EraRule "SNAP" era))

deriving instance
  ( Eq (PredicateFailure (EraRule "RATIFY" era))
  , Eq (PredicateFailure (EraRule "SNAP" era))
  , Eq (PredicateFailure (EraRule "POOLREAP" era))
  ) =>
  Eq (ConwayEpochPredFailure era)

deriving instance
  ( Show (PredicateFailure (EraRule "RATIFY" era))
  , Show (PredicateFailure (EraRule "SNAP" era))
  , Show (PredicateFailure (EraRule "POOLREAP" era))
  ) =>
  Show (ConwayEpochPredFailure era)

instance
  ( EraTxOut era
  , EraGovernance era
  , Embed (EraRule "SNAP" era) (ConwayEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era)
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ConwayEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ PParams era
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Eq (UpecPredFailure era)
  , Show (UpecPredFailure era)
  , Embed (EraRule "RATIFY" era) (ConwayEPOCH era)
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , GovernanceState era ~ ConwayGovernance era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  ) =>
  STS (ConwayEPOCH era)
  where
  type State (ConwayEPOCH era) = EpochState era
  type Signal (ConwayEPOCH era) = EpochNo
  type Environment (ConwayEPOCH era) = PoolDistr (EraCrypto era)
  type BaseM (ConwayEPOCH era) = ShelleyBase
  type PredicateFailure (ConwayEPOCH era) = ConwayEpochPredFailure era
  type Event (ConwayEPOCH era) = ConwayEpochEvent era
  transitionRules = [epochTransition]

epochTransition ::
  forall era.
  ( Embed (EraRule "SNAP" era) (ConwayEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era)
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ConwayEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ PParams era
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "RATIFY" era) (ConwayEPOCH era)
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , GovernanceState era ~ ConwayGovernance era
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  ) =>
  TransitionRule (ConwayEPOCH era)
epochTransition = do
  TRC
    ( stakePoolDistr
      , EpochState
          { esAccountState = acnt
          , esSnapshots = ss
          , esLState = ls
          , esPrevPp = pr
          , esPp = pp
          , esNonMyopic = nm
          }
      , eNo
      ) <-
    judgmentContext
  let utxoSt = lsUTxOState ls
  let CertState vstate pstate dstate = lsCertState ls
  ss' <-
    trans @(EraRule "SNAP" era) $ TRC (SnapEnv ls pp, ss, ())

  let PState pParams fPParams _ _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { psStakePoolParams = ppp
          , psFutureStakePoolParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(EraRule "POOLREAP" era) $
      TRC (pp, PoolreapState utxoSt acnt dstate pstate', eNo)

  let adjustedDPstate = CertState vstate pstate'' dstate'
      epochState' =
        EpochState
          acnt'
          ss'
          (ls {lsUTxOState = utxoSt', lsCertState = adjustedDPstate})
          pr
          pp
          nm

  let
    utxoSt''' = utxoSt' {utxosDeposited = obligationCertState adjustedDPstate}
    acnt'' = acnt' {asReserves = asReserves acnt'}
    govSt = utxosGovernance utxoSt'''
    stakeDistr = credMap $ utxosStakeDistr utxoSt'''
    ratEnv =
      RatifyEnv
        { reStakeDistr = stakeDistr
        , reStakePoolDistr = stakePoolDistr
        , reCurrentEpoch = eNo
        }
    tallyStateToSeq = Seq.fromList . Map.toList
    ratSig = RatifySignal . tallyStateToSeq . unConwayTallyState $ cgTally govSt
  rs@RatifyState {rsFuture} <-
    trans @(EraRule "RATIFY" era) $ TRC (ratEnv, cgRatify govSt, ratSig)
  let es'' =
        epochState'
          { esAccountState = acnt''
          , esLState = (esLState epochState') {lsUTxOState = utxoSt'''}
          , esPrevPp = pp
          , esPp = pp
          }
      newTally = ConwayTallyState . Map.fromList . toList $ rsFuture
  pure $
    (es'' & esLStateL . lsUTxOStateL . utxosGovernanceL . cgTallyL .~ newTally)
      & (esLStateL . lsUTxOStateL . utxosGovernanceL . cgRatifyL .~ rs)

instance
  ( Era era
  , STS (ShelleyPOOLREAP era)
  , PredicateFailure (EraRule "POOLREAP" era) ~ ShelleyPoolreapPredFailure era
  , Event (EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  ) =>
  Embed (ShelleyPOOLREAP era) (ConwayEPOCH era)
  where
  wrapFailed = ConwayPoolReapFailure
  wrapEvent = PoolReapEvent

instance
  ( EraTxOut era
  , PredicateFailure (EraRule "SNAP" era) ~ ShelleySnapPredFailure era
  , Event (EraRule "SNAP" era) ~ Shelley.SnapEvent era
  ) =>
  Embed (ShelleySNAP era) (ConwayEPOCH era)
  where
  wrapFailed = ConwaySnapFailure
  wrapEvent = SnapEvent

instance
  ( EraGovernance era
  , STS (ConwayRATIFY era)
  , BaseM (ConwayRATIFY era) ~ ShelleyBase
  , Event (ConwayRATIFY era) ~ Void
  , PredicateFailure (EraRule "RATIFY" era) ~ EnactPredFailure era
  ) =>
  Embed (ConwayRATIFY era) (ConwayEPOCH era)
  where
  wrapFailed = ConwayRatifyFailure
  wrapEvent = absurd
