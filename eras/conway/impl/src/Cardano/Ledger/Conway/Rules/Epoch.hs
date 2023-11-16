{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  CertState (..),
  CommitteeState (..),
  VState,
  dsUnifiedL,
  vsCommitteeStateL,
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Coin (compactCoinOrError)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEPOCH, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  Committee,
  ConwayEraGov,
  ConwayGovState (..),
  DRepPulsingState (..),
  EnactState,
  GovActionState (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  RunConwayRatify,
  cgEnactStateL,
  cgProposalsL,
  dormantEpoch,
  ensCommitteeL,
  ensPrevPParamsL,
  ensTreasuryL,
  ensWithdrawalsL,
  epochStateDRepPulsingStateL,
  extractDRepPulsingState,
  setFreshDRepPulsingState,
 )
import Cardano.Ledger.Conway.Governance.Procedures (Committee (..))
import Cardano.Ledger.Conway.Governance.Proposals (proposalsRemoveDescendentIds, proposalsRemoveIds)
import Cardano.Ledger.EpochBoundary (SnapShots)
import Cardano.Ledger.PoolDistr (PoolDistr)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  PState (..),
  UTxOState (..),
  asTreasuryL,
  epochStateGovStateL,
  esAccountState,
  esAccountStateL,
  esLStateL,
  esSnapshotsL,
  lsCertStateL,
  lsUTxOStateL,
  totalObligation,
  utxosDepositedL,
  utxosDonationL,
  utxosGovStateL,
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
import Cardano.Ledger.UMap (UView (..), unionRewAgg, (∪+), (◁))
import Cardano.Ledger.Val (zero, (<->))
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition (
  Assertion (..),
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  tellEvent,
  trans,
 )
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Void (Void, absurd)
import Lens.Micro ((%~), (&), (+~), (.~), (<>~), (^.))

-- ====================================================

data ConwayEpochEvent era
  = PoolReapEvent (Event (EraRule "POOLREAP" era))
  | SnapEvent (Event (EraRule "SNAP" era))
  | EpochBoundaryRatifyState (RatifyState era)

instance
  ( EraTxOut era
  , RunConwayRatify era
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

  -- EPOCH rule can never fail
  type PredicateFailure (ConwayEPOCH era) = Void
  type Event (ConwayEPOCH era) = ConwayEpochEvent era
  transitionRules = [epochTransition]
  assertions =
    let withdrawalsEmptyMessage = "Withdrawals in EnactState must be empty"
        withdrawalsEmptyCheck es = null $ es ^. epochStateGovStateL . cgEnactStateL . ensWithdrawalsL
        treasuryZeroMessage = "Treasury in EnactState must be zero"
        treasuryZeroCheck es = zero == es ^. epochStateGovStateL . cgEnactStateL . ensTreasuryL
     in [ PreCondition withdrawalsEmptyMessage (\(TRC (_, es, _)) -> withdrawalsEmptyCheck es)
        , PostCondition withdrawalsEmptyMessage (const withdrawalsEmptyCheck)
        , PreCondition treasuryZeroMessage (\(TRC (_, es, _)) -> treasuryZeroCheck es)
        , PostCondition treasuryZeroMessage (const treasuryZeroCheck)
        ]

returnProposalDeposits ::
  Foldable f => f (GovActionState era) -> DState era -> DState era
returnProposalDeposits removedProposals =
  dsUnifiedL %~ returnProposalDepositsUMap
  where
    returnProposalDepositsUMap umap =
      unionRewAgg (RewDepUView umap) $ foldl' addReward mempty removedProposals
      where
        addReward m' GovActionState {..} =
          Map.insertWith
            (<>)
            (getRwdCred gasReturnAddr)
            (compactCoinOrError gasDeposit) -- Deposits have been validated at this point
            m'

-- | When there have been zero governance proposals to vote on in the previous epoch
-- increase the dormant-epoch counter by one.
updateNumDormantEpochs :: DRepPulsingState era -> VState era -> VState era
updateNumDormantEpochs pulser vState =
  if dormantEpoch pulser
    then vState & vsNumDormantEpochsL +~ 1
    else vState

-- | Apply TreasuryWithdrawals to the EpochState
--
--   acnt' = record acnt { treasury = treasury + UTxOState.fees utxoSt
--                                  + getCoin unclaimed + donations ∸ totWithdrawals }
--
-- The utxo fees and donations are applied in the remaining body of EPOCH transition
applyEnactedWithdrawals ::
  AccountState ->
  DState era ->
  EnactState era ->
  (AccountState, DState era, EnactState era)
applyEnactedWithdrawals accountState dState enactedState =
  let enactedWithdrawals = enactedState ^. ensWithdrawalsL
      rewardsUView = RewDepUView $ dState ^. dsUnifiedL
      successfulWithdrawls = rewardsUView ◁ enactedWithdrawals
      accountState' =
        accountState
          -- Subtract `successfulRefunds` from the treasury, and add them to the rewards UMap
          -- `unclaimed` refunds remain in the treasury.
          -- Compared to the spec, instead of adding `unclaimed` and subtracting `totWithdrawals`
          --   + unclaimed - totWithdrawals
          -- we just subtract the `refunds`
          --   - refunds
          & asTreasuryL %~ (<-> fold successfulWithdrawls)
      -- The use of the partial function `compactCoinOrError` is justified here because
      -- 1. the decoder for coin at the proposal-submission boundary has already
      --    confirmed we have a compactible value
      -- 2. the refunds and unsuccessful refunds together do not exceed the
      --    current treasury value, as enforced by the `ENACT` rule.
      dState' =
        dState
          & dsUnifiedL .~ (rewardsUView ∪+ (compactCoinOrError <$> successfulWithdrawls))
      -- Reset enacted withdrawals:
      enactedState' =
        enactedState
          & ensWithdrawalsL .~ Map.empty
          & ensTreasuryL .~ mempty
   in (accountState', dState', enactedState')

epochTransition ::
  forall era.
  ( RunConwayRatify era
  , Embed (EraRule "SNAP" era) (ConwayEPOCH era)
  , EraTxOut era
  , Eq (UpecPredFailure era)
  , Show (UpecPredFailure era)
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
      , epochState0@EpochState
          { esAccountState = accountState0
          , esSnapshots = snapshots0
          , esLState = ledgerState0
          }
      , eNo
      ) <-
    judgmentContext
  let govState0 = utxosGovState utxoState0
      curPParams = govState0 ^. curPParamsGovStateL
      utxoState0 = lsUTxOState ledgerState0
      CertState vState pState0 dState0 = lsCertState ledgerState0
  snapshots1 <-
    trans @(EraRule "SNAP" era) $ TRC (SnapEnv ledgerState0 curPParams, snapshots0, ())

  -- Activate future StakePols
  let newStakePoolParams = eval (psStakePoolParams pState0 ⨃ psFutureStakePoolParams pState0)
      pState1 =
        pState0
          { psStakePoolParams = newStakePoolParams
          , psFutureStakePoolParams = Map.empty
          }
  PoolreapState utxoState1 accountState1 dState1 pState2 <-
    trans @(EraRule "POOLREAP" era) $
      TRC (ShelleyPoolreapEnv vState, PoolreapState utxoState0 accountState0 dState0 pState1, eNo)

  let
    pulsingState = epochState0 ^. epochStateDRepPulsingStateL

    ratState0@RatifyState {rsRemoved, rsEnacted, rsEnactState} = extractDRepPulsingState pulsingState

    (accountState2, dState2, newEnactState) =
      applyEnactedWithdrawals accountState1 dState1 rsEnactState

    -- It is important that we use current proposals here instead of the ones from the pulser
    (proposals0, removedInvalidGovActions) = proposalsRemoveDescendentIds rsRemoved (govState0 ^. cgProposalsL)
    (proposals1, removedEnactedGovActions) = proposalsRemoveIds rsEnacted proposals0

    govState1 =
      govState0
        & cgProposalsL .~ proposals1
        & cgEnactStateL .~ (newEnactState & ensPrevPParamsL .~ curPParams)

    allRemovedGovActions = removedInvalidGovActions <> removedEnactedGovActions

    certState =
      CertState
        { certPState = pState2
        , certDState = returnProposalDeposits allRemovedGovActions dState2
        , certVState =
            -- Increment the dormant epoch counter
            updateNumDormantEpochs pulsingState vState
              -- Remove cold credentials of committee members that were removed or were invalid
              & vsCommitteeStateL %~ updateCommitteeState (govState1 ^. cgEnactStateL . ensCommitteeL)
        }
    accountState3 =
      accountState2
        -- Move donations to treasury:
        & asTreasuryL <>~ (utxoState0 ^. utxosDonationL)
    utxoState2 =
      utxoState1
        & utxosDepositedL .~ totalObligation certState govState1
        -- Clear the donations field:
        & utxosDonationL .~ zero
        & utxosGovStateL .~ govState1
    ledgerState1 =
      ledgerState0
        & lsCertStateL .~ certState
        & lsUTxOStateL .~ utxoState2
    epochState1 =
      epochState0
        & esAccountStateL .~ accountState3
        & esSnapshotsL .~ snapshots1
        & esLStateL .~ ledgerState1
  tellEvent $ EpochBoundaryRatifyState ratState0
  liftSTS $ setFreshDRepPulsingState eNo stakePoolDistr epochState1

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
  , PredicateFailure (ConwayRATIFY era) ~ Void
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
  CommitteeState $ Map.intersection creds members
  where
    members = foldMap' committeeMembers committee
