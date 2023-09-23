{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Epoch (
  ConwayEPOCH,
  PredicateFailure,
  ConwayEpochEvent (..),
)
where

import Cardano.Ledger.Address (RewardAcnt (getRwdCred))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.CertState (
  CommitteeState (..),
  certDStateL,
  certVStateL,
  dsUnifiedL,
  vsCommitteeStateL,
  vsDRepsL,
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Coin (Coin, compactCoinOrError)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEPOCH, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  Committee,
  ConwayEraGov,
  ConwayGovState (..),
  EnactState,
  GovActionState (..),
  GovSnapshots (..),
  RatifyState (..),
  cgEnactStateL,
  cgGovSnapshotsL,
  curGovSnapshotsL,
  ensCommitteeL,
  ensTreasuryL,
  ensWithdrawalsL,
  epochStateDRepDistrL,
  prevGovSnapshotsL,
  snapshotActions,
  snapshotIds,
  snapshotsGovStateL,
 )
import Cardano.Ledger.Conway.Governance.Procedures (Committee (..))
import Cardano.Ledger.Conway.Governance.Snapshots (snapshotLookupId, snapshotRemoveIds)
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
  asTreasuryL,
  curPParamsEpochStateL,
  epochStateTreasuryL,
  epochStateUMapL,
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
  prevPParamsEpochStateL,
  totalObligation,
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
import Cardano.Ledger.UMap (UMap, UView (..), unionKeyDeposits, (∪+), (◁))
import Cardano.Ledger.Val ((<->))
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
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Lens.Micro (Lens', (%~), (&), (+~), (.~), (<>~), (^.))

data ConwayEpochEvent era
  = PoolReapEvent (Event (EraRule "POOLREAP" era))
  | SnapEvent (Event (EraRule "SNAP" era))

instance
  ( EraTxOut era
  , ConwayEraGov era
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

  -- \| EPOCH should never fail
  type PredicateFailure (ConwayEPOCH era) = Void
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
  StrictSeq (GovActionState era) ->
  LedgerState era ->
  LedgerState era
returnProposalDeposits removedProposals =
  lsCertStateL . certDStateL . dsUnifiedL %~ updateUMap
  where
    updateUMap = returnProposalDepositsUMap removedProposals

-- | When there have been zero governance proposals to vote on in the previous epoch
-- increase the dormant-epoch counter by one.
updateNumDormantEpochs :: ConwayEraGov era => LedgerState era -> LedgerState era
updateNumDormantEpochs ls =
  let wasPrevEpochDormant = Seq.null . snapshotIds $ ls ^. lsUTxOStateL . utxosGovStateL . snapshotsGovStateL . prevGovSnapshotsL
   in if wasPrevEpochDormant
        then ls & lsCertStateL . certVStateL . vsNumDormantEpochsL +~ 1
        else ls

-- | Apply TreasuryWithdrawals to the EpochState
--
--   acnt' = record acnt { treasury = treasury + UTxOState.fees utxoSt
--                                  + getCoin unclaimed + donations ∸ totWithdrawals }
--
-- The utxo fees and donations are applied in the remaining body of EPOCH transition
applyEnactedWithdrawals :: EpochState era -> EnactState era -> (EpochState era, EnactState era)
applyEnactedWithdrawals epochState enactedState =
  let enactedWithdrawals = enactedState ^. ensWithdrawalsL
      rewardsUView = RewDepUView $ epochState ^. epochStateUMapL
      successfulRefunds = rewardsUView ◁ enactedWithdrawals
      epochStateRewardsApplied =
        epochState
          -- Subtract `successfulRefunds` from the treasury, and add them to the rewards UMap
          -- `unclaimed` refunds remain in the treasury.
          -- Compared to the spec, instead of adding `unclaimed` and subtracting `totWithdrawals`
          --   + unclaimed - totWithdrawals
          -- we just subtract the `refunds`
          --   - refunds
          & epochStateTreasuryL %~ (<-> fold successfulRefunds)
          -- The use of the partial function `compactCoinOrError` is justified here because
          -- 1. the decoder for coin at the proposal-submission boundary has already confirmed we have a compactible value
          -- 2. the refunds and unsuccessful refunds together do not exceed the current treasury value, as enforced by the `ENACT` rule.
          & epochStateUMapL .~ (rewardsUView ∪+ (compactCoinOrError <$> successfulRefunds))
      enactedStateWithdrawalsReset = enactedState & ensWithdrawalsL .~ Map.empty -- reset enacted withdrawals
   in (epochStateRewardsApplied, enactedStateWithdrawalsReset)

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
  , ConwayEraGov era
  ) =>
  TransitionRule (ConwayEPOCH era)
epochTransition = do
  TRC
    ( stakePoolDistr
      , es@EpochState
          { esAccountState = acnt
          , esSnapshots = ss
          , esLState = ledgerState
          , esNonMyopic = nm
          }
      , eNo
      ) <-
    judgmentContext
  let ls = updateNumDormantEpochs ledgerState
      pp = es ^. curPParamsEpochStateL
      utxoSt = lsUTxOState ls
      CertState vstate pstate dstate = lsCertState ls
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
      es' =
        EpochState
          acnt'
          adjustedLState
          ss'
          nm

  let
    utxoSt'' =
      utxoSt'
        { utxosDeposited =
            totalObligation
              adjustedCertState
              (utxoSt' ^. utxosGovStateL)
        }
    govSt = utxosGovState utxoSt''
    stakeDistr = credMap $ utxosStakeDistr utxoSt''
    drepDistr = extractDRepDistr (es' ^. epochStateDRepDistrL)
    ratEnv =
      RatifyEnv
        { reStakeDistr = stakeDistr
        , reStakePoolDistr = stakePoolDistr
        , reDRepDistr = drepDistr
        , reCurrentEpoch = eNo - 1
        , reDRepState = vstate ^. vsDRepsL
        , reCommitteeState = vstate ^. vsCommitteeStateL
        }
    ratSig = RatifySignal . snapshotActions . prevGovSnapshots $ cgGovSnapshots govSt
  RatifyState {rsRemoved, rsEnactState = rsEnactState'} <-
    trans @(EraRule "RATIFY" era) $
      TRC
        ( ratEnv
        , RatifyState
            { rsRemoved = mempty
            , rsEnactState = (govSt ^. cgEnactStateL) & ensTreasuryL .~ (es' ^. epochStateTreasuryL)
            , rsDelayed = False
            }
        , ratSig
        )
  let
    (epochState', rsEnactState) = applyEnactedWithdrawals es' rsEnactState'
    curSnaps = curGovSnapshots $ cgGovSnapshots govSt
    lookupAction m gId =
      case snapshotLookupId gId curSnaps of
        Just x -> x :<| m
        Nothing -> m
    removedProps =
      foldl' lookupAction mempty $
        Seq.fromList (Set.toList rsRemoved)
    lState = returnProposalDeposits removedProps $ esLState epochState'
    es'' =
      epochState'
        { esLState = lState {lsUTxOState = utxoSt''}
        }
        & prevPParamsEpochStateL .~ pp
        & curPParamsEpochStateL .~ pp
    updatedCommitteeState =
      updateCommitteeState
        (rsEnactState ^. ensCommitteeL)
        (es' ^. esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL)
    esGovernanceL :: Lens' (EpochState era) (GovState era)
    esGovernanceL = esLStateL . lsUTxOStateL . utxosGovStateL
    oldGovSnapshots =
      es ^. esGovernanceL . cgGovSnapshotsL . curGovSnapshotsL
    newGovSnapshots =
      snapshotRemoveIds
        rsRemoved
        oldGovSnapshots
    newGov =
      GovSnapshots
        { -- We set both curGovSnapshots and prevGovSnapshots to the same
          -- value at the epoch boundary. We only change curGovSnapshots
          -- during the next epoch while prevGovSnapshots stays unchanged
          -- so we can tally the votes from the previous epoch.
          curGovSnapshots = newGovSnapshots
        , prevGovSnapshots = newGovSnapshots
        , prevDRepsState =
            es'
              ^. esLStateL . lsCertStateL . certVStateL . vsDRepsL
        , prevCommitteeState =
            es'
              ^. esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
        }
    esDonationL :: Lens' (EpochState era) Coin
    esDonationL = esLStateL . lsUTxOStateL . utxosDonationL
    donations = es'' ^. esDonationL
  pure $
    es''
      & esGovernanceL . cgEnactStateL .~ rsEnactState
      & esGovernanceL . cgGovSnapshotsL .~ newGov
      -- Move donations to treasury
      & esAccountStateL . asTreasuryL <>~ donations
      -- remove hot keys of committee members that were removed
      & esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL .~ updatedCommitteeState
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
  wrapFailed = \case {}
  wrapEvent = PoolReapEvent

instance
  ( EraTxOut era
  , PredicateFailure (EraRule "SNAP" era) ~ ShelleySnapPredFailure era
  , Event (EraRule "SNAP" era) ~ Shelley.SnapEvent era
  ) =>
  Embed (ShelleySNAP era) (ConwayEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = SnapEvent

instance
  ( EraGov era
  , STS (ConwayRATIFY era)
  , BaseM (ConwayRATIFY era) ~ ShelleyBase
  , Event (ConwayRATIFY era) ~ Void
  ) =>
  Embed (ConwayRATIFY era) (ConwayEPOCH era)
  where
  wrapFailed = absurd
  wrapEvent = absurd

updateCommitteeState :: StrictMaybe (Committee era) -> CommitteeState era -> CommitteeState era
updateCommitteeState committee (CommitteeState creds) =
  CommitteeState $ Map.difference creds members
  where
    members = foldMap' committeeMembers committee
