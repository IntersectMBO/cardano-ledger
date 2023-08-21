{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
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

import Cardano.Ledger.Address (RewardAcnt (getRwdCred))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.CertState (certDStateL, dsUnifiedL)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEPOCH, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  ConwayGovState (..),
  GovActionState (..),
  GovActionsState (..),
  RatifyState (..),
  cgGovActionsStateL,
  cgRatifyStateL,
  insertGovActionsState,
 )
import Cardano.Ledger.Conway.Rules.Enact (EnactPredFailure)
import Cardano.Ledger.Conway.Rules.Ratify (RatifyEnv (..), RatifySignal (..))
import Cardano.Ledger.DRepDistr (extractDRepDistr)
import Cardano.Ledger.EpochBoundary (SnapShots)
import Cardano.Ledger.PoolDistr (PoolDistr)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  IncrementalStake (..),
  LedgerState (..),
  PState (..),
  UTxOState (..),
  asReserves,
  asTreasuryL,
  curPParamsEpochStateL,
  epochStateDRepDistrL,
  esAccountState,
  esAccountStateL,
  esLState,
  esLStateL,
  esNonMyopic,
  esSnapshots,
  lsCertState,
  lsCertStateL,
  lsUTxOState,
  lsUTxOStateL,
  obligationCertState,
  prevPParamsEpochStateL,
  utxosDonationL,
  utxosGovStateL,
  pattern CertState,
  pattern EpochState,
 )
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyPOOLREAP,
  ShelleyPoolreapEnv (..),
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
import Cardano.Ledger.UMap (UMap, UView (..), unionKeyDeposits)
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
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq
import Data.Void (Void, absurd)
import Lens.Micro (Lens', (%~), (&), (.~), (<>~), (^.))

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
  , EraGov era
  , Embed (EraRule "SNAP" era) (ConwayEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era)
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ConwayEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ ShelleyPoolreapEnv era
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Eq (UpecPredFailure era)
  , Show (UpecPredFailure era)
  , Embed (EraRule "RATIFY" era) (ConwayEPOCH era)
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , GovState era ~ ConwayGovState era
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

returnProposalDepositsUMap ::
  StrictSeq (GovActionState era) ->
  UMap (EraCrypto era) ->
  UMap (EraCrypto era)
returnProposalDepositsUMap gaids m =
  unionKeyDeposits (RewDepUView m) $ foldl' addRew mempty gaids
  where
    addRew m' GovActionState {..} =
      Map.insertWith
        (<>)
        (getRwdCred gasReturnAddr)
        (fromMaybe mempty $ toCompact gasDeposit)
        m'

returnProposalDeposits ::
  forall era.
  GovState era ~ ConwayGovState era =>
  LedgerState era ->
  LedgerState era
returnProposalDeposits ls@LedgerState {..} =
  ls
    & lsCertStateL . certDStateL . dsUnifiedL %~ dstate
  where
    govSt = utxosGovState lsUTxOState
    ratifyState = cgRatifyState govSt
    removedProposals = rsRemoved ratifyState
    dstate = returnProposalDepositsUMap removedProposals

epochTransition ::
  forall era.
  ( Embed (EraRule "SNAP" era) (ConwayEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era)
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ConwayEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ ShelleyPoolreapEnv era
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "RATIFY" era) (ConwayEPOCH era)
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , GovState era ~ ConwayGovState era
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  , EraGov era
  ) =>
  TransitionRule (ConwayEPOCH era)
epochTransition = do
  TRC
    ( stakePoolDistr
      , es@EpochState
          { esAccountState = acnt
          , esSnapshots = ss
          , esLState = ls
          , esNonMyopic = nm
          }
      , eNo
      ) <-
    judgmentContext
  let pp = es ^. curPParamsEpochStateL
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
      TRC (ShelleyPoolreapEnv vstate, PoolreapState utxoSt acnt dstate pstate', eNo)

  let adjustedCertState = CertState vstate pstate'' dstate'
      adjustedLState =
        ls
          { lsUTxOState = utxoSt'
          , lsCertState = adjustedCertState
          }
      epochState' =
        EpochState
          acnt'
          (returnProposalDeposits adjustedLState)
          ss'
          nm

  let
    utxoSt''' = utxoSt' {utxosDeposited = obligationCertState adjustedCertState}
    acnt'' = acnt' {asReserves = asReserves acnt'}
    govSt = utxosGovState utxoSt'''
    stakeDistr = credMap $ utxosStakeDistr utxoSt'''
    drepDistr = extractDRepDistr (epochState' ^. epochStateDRepDistrL)
    ratEnv =
      RatifyEnv
        { reStakeDistr = stakeDistr
        , reStakePoolDistr = stakePoolDistr
        , reDRepDistr = drepDistr
        , reCurrentEpoch = eNo
        }
    ratSig =
      RatifySignal . Seq.fromList . Map.elems . unGovActionsState $
        cgGovActionsState govSt
  rs@RatifyState {rsFuture} <-
    trans @(EraRule "RATIFY" era) $ TRC (ratEnv, cgRatifyState govSt, ratSig)
  let es'' =
        epochState'
          { esAccountState = acnt''
          , esLState = (esLState epochState') {lsUTxOState = utxoSt'''}
          }
          & prevPParamsEpochStateL .~ pp
          & curPParamsEpochStateL .~ pp
      -- TODO can we be more efficient?
      newGov = foldr' insertGovActionsState mempty rsFuture
      esGovernanceL = esLStateL . lsUTxOStateL . utxosGovStateL
      esDonationL :: Lens' (EpochState era) Coin
      esDonationL = esLStateL . lsUTxOStateL . utxosDonationL
      donations = es'' ^. esDonationL
  pure $
    es''
      & esGovernanceL . cgGovActionsStateL .~ newGov
      & esGovernanceL . cgRatifyStateL .~ rs
      -- Move donations to treasury
      & esAccountStateL . asTreasuryL <>~ donations
      -- Clear the donations field
      & esDonationL .~ mempty

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
  ( EraGov era
  , STS (ConwayRATIFY era)
  , BaseM (ConwayRATIFY era) ~ ShelleyBase
  , Event (ConwayRATIFY era) ~ Void
  , PredicateFailure (EraRule "RATIFY" era) ~ EnactPredFailure era
  ) =>
  Embed (ConwayRATIFY era) (ConwayEPOCH era)
  where
  wrapFailed = ConwayRatifyFailure
  wrapEvent = absurd
