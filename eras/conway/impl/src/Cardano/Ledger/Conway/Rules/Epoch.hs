{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
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
)
where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase)
import Cardano.Ledger.CertState (
  CommitteeState (..),
  EraCertState (..),
  VState,
  dsUnifiedL,
  vsCommitteeStateL,
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Coin (Coin, compactCoinOrError)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEPOCH, ConwayEra, ConwayHARDFORK, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  Committee,
  ConwayEraGov (..),
  ConwayGovState,
  EnactState (..),
  GovActionId,
  GovActionState (..),
  Proposals,
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  RunConwayRatify,
  cgsCommitteeL,
  cgsConstitutionL,
  cgsCurPParamsL,
  cgsFuturePParamsL,
  cgsPrevPParamsL,
  cgsProposalsL,
  ensTreasuryL,
  ensWithdrawalsL,
  epochStateDRepPulsingStateL,
  extractDRepPulsingState,
  gasDeposit,
  gasReturnAddr,
  pPropsL,
  proposalsApplyEnactment,
  proposalsGovStateL,
  setFreshDRepPulsingState,
 )
import Cardano.Ledger.Conway.Governance.Procedures (Committee (..))
import Cardano.Ledger.Conway.Rules.HardFork (
  ConwayHardForkEvent (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  PState (..),
  UTxOState (..),
  asTreasuryL,
  curPParamsEpochStateL,
  esAccountState,
  esAccountStateL,
  esLStateL,
  esSnapshotsL,
  lsCertStateL,
  lsUTxOStateL,
  prevPParamsEpochStateL,
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
import Cardano.Ledger.State (SnapShots (..))
import Cardano.Ledger.UMap (RDPair (..), UMap, UView (..), (∪+), (◁))
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Ledger.Val (zero, (<->))
import Control.DeepSeq (NFData)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition (
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
import qualified Data.OMap.Strict as OMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~), (<>~), (^.))

data ConwayEpochEvent era
  = PoolReapEvent (Event (EraRule "POOLREAP" era))
  | SnapEvent (Event (EraRule "SNAP" era))
  | EpochBoundaryRatifyState (RatifyState era)
  | GovInfoEvent
      -- | Enacted actions
      (Set (GovActionState era))
      -- | Actions that were removed as conflicting due to enactment
      (Set (GovActionState era))
      -- | Actions that were removed due to expiration together with their dependees
      (Set (GovActionState era))
      -- | Map of removed governance action ids that had an unregistered reward account to their unclaimed deposits so they can be transferred to the treasury.
      (Map.Map GovActionId Coin)
  | HardForkEvent (Event (EraRule "HARDFORK" era))
  deriving (Generic)

type instance EraRuleEvent "EPOCH" ConwayEra = ConwayEpochEvent ConwayEra

deriving instance
  ( EraPParams era
  , Eq (Event (EraRule "POOLREAP" era))
  , Eq (Event (EraRule "SNAP" era))
  , Eq (Event (EraRule "HARDFORK" era))
  ) =>
  Eq (ConwayEpochEvent era)

instance
  ( EraPParams era
  , NFData (Event (EraRule "POOLREAP" era))
  , NFData (Event (EraRule "SNAP" era))
  , NFData (Event (EraRule "HARDFORK" era))
  ) =>
  NFData (ConwayEpochEvent era)

instance
  ( EraTxOut era
  , RunConwayRatify era
  , ConwayEraGov era
  , Embed (EraRule "SNAP" era) (ConwayEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots
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
  , Embed (EraRule "HARDFORK" era) (ConwayEPOCH era)
  , Environment (EraRule "HARDFORK" era) ~ ()
  , State (EraRule "HARDFORK" era) ~ EpochState era
  , Signal (EraRule "HARDFORK" era) ~ ProtVer
  , EraCertState era
  ) =>
  STS (ConwayEPOCH era)
  where
  type State (ConwayEPOCH era) = EpochState era
  type Signal (ConwayEPOCH era) = EpochNo
  type Environment (ConwayEPOCH era) = ()
  type BaseM (ConwayEPOCH era) = ShelleyBase

  -- EPOCH rule can never fail
  type PredicateFailure (ConwayEPOCH era) = Void
  type Event (ConwayEPOCH era) = ConwayEpochEvent era
  transitionRules = [epochTransition]

returnProposalDeposits ::
  Foldable f =>
  f (GovActionState era) ->
  UMap ->
  (UMap, Map.Map GovActionId Coin)
returnProposalDeposits removedProposals oldUMap =
  foldr' processProposal (oldUMap, mempty) removedProposals
  where
    processProposal gas (um, unclaimed)
      | UMap.member (raCredential (gasReturnAddr gas)) (RewDepUView um) =
          ( UMap.adjust
              (addReward (gasDeposit gas))
              (raCredential (gasReturnAddr gas))
              (RewDepUView um)
          , unclaimed
          )
      | otherwise = (um, Map.insert (gasId gas) (gasDeposit gas) unclaimed)
    addReward c rd =
      -- Deposits have been validated at this point
      rd {rdReward = rdReward rd <> compactCoinOrError c}

-- | When there have been zero governance proposals to vote on in the previous epoch
-- increase the dormant-epoch counter by one.
updateNumDormantEpochs :: EpochNo -> Proposals era -> VState era -> VState era
updateNumDormantEpochs currentEpoch ps vState =
  if null $ OMap.filter ((currentEpoch <=) . gasExpiresAfter) $ ps ^. pPropsL
    then vState & vsNumDormantEpochsL %~ succ
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
          -- Subtract `successfulWithdrawals` from the treasury, and add them to the rewards UMap
          -- `unclaimed` withdrawals remain in the treasury.
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
  , State (EraRule "SNAP" era) ~ SnapShots
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
  , Embed (EraRule "HARDFORK" era) (ConwayEPOCH era)
  , Environment (EraRule "HARDFORK" era) ~ ()
  , State (EraRule "HARDFORK" era) ~ EpochState era
  , Signal (EraRule "HARDFORK" era) ~ ProtVer
  , EraCertState era
  ) =>
  TransitionRule (ConwayEPOCH era)
epochTransition = do
  TRC
    ( ()
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
      certState0 = ledgerState0 ^. lsCertStateL
      vState = certState0 ^. certVStateL
      pState0 = certState0 ^. certPStateL
      dState0 = certState0 ^. certDStateL
  snapshots1 <-
    trans @(EraRule "SNAP" era) $ TRC (SnapEnv ledgerState0 curPParams, snapshots0, ())

  -- Activate future StakePools
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
    stakePoolDistr = ssStakeMarkPoolDistr snapshots1
    pulsingState = epochState0 ^. epochStateDRepPulsingStateL

    ratifyState@RatifyState {rsEnactState, rsEnacted, rsExpired} =
      extractDRepPulsingState pulsingState

    (accountState2, dState2, EnactState {..}) =
      applyEnactedWithdrawals accountState1 dState1 rsEnactState

    -- NOTE: It is important that we apply the results of ratification
    -- and enactment from the pulser to the working copy of proposals.
    -- The proposals in the pulser are a subset of the current
    -- proposals, in that, in addition to the proposals in the pulser,
    -- the current proposals now contain new proposals submitted during
    -- the epoch that just passed (we are at its boundary here) and
    -- any votes that were submitted to the already pulsing as well as
    -- newly submitted proposals. We only need to apply the enactment
    -- operations to this superset to get a new set of proposals with:
    -- enacted actions and their sibling subtrees, as well as expired
    -- actions and their subtrees, removed, and with all the votes
    -- intact for the rest of them.
    (newProposals, enactedActions, removedDueToEnactment, expiredActions) =
      proposalsApplyEnactment rsEnacted rsExpired (govState0 ^. proposalsGovStateL)

    -- Apply the values from the computed EnactState to the GovState
    govState1 =
      govState0
        & cgsProposalsL .~ newProposals
        & cgsCommitteeL .~ ensCommittee
        & cgsConstitutionL .~ ensConstitution
        & cgsCurPParamsL .~ nextEpochPParams govState0
        & cgsPrevPParamsL .~ curPParams
        & cgsFuturePParamsL .~ PotentialPParamsUpdate Nothing

    allRemovedGovActions = Map.unions [expiredActions, enactedActions, removedDueToEnactment]
    (newUMap, unclaimed) =
      returnProposalDeposits allRemovedGovActions $
        dState2 ^. dsUnifiedL

  tellEvent $
    GovInfoEvent
      (Set.fromList $ Map.elems enactedActions)
      (Set.fromList $ Map.elems removedDueToEnactment)
      (Set.fromList $ Map.elems expiredActions)
      unclaimed

  let
    certState1 =
      mkCertState
        -- Increment the dormant epoch counter
        (updateNumDormantEpochs eNo newProposals vState)
        pState2
        dState2
        & certDStateL . dsUnifiedL .~ newUMap
        -- Remove cold credentials of committee members that were removed or were invalid
        & certVStateL . vsCommitteeStateL %~ updateCommitteeState (govState1 ^. cgsCommitteeL)
    accountState3 =
      accountState2
        -- Move donations and unclaimed rewards from proposals to treasury:
        & asTreasuryL <>~ (utxoState0 ^. utxosDonationL <> fold unclaimed)
    utxoState2 =
      utxoState1
        & utxosDepositedL .~ totalObligation certState1 govState1
        -- Clear the donations field:
        & utxosDonationL .~ zero
        & utxosGovStateL .~ govState1
    ledgerState1 =
      ledgerState0
        & lsCertStateL .~ certState1
        & lsUTxOStateL .~ utxoState2
    epochState1 =
      epochState0
        & esAccountStateL .~ accountState3
        & esSnapshotsL .~ snapshots1
        & esLStateL .~ ledgerState1
  tellEvent $ EpochBoundaryRatifyState ratifyState
  epochState2 <- do
    let curPv = epochState1 ^. curPParamsEpochStateL . ppProtocolVersionL
    if curPv /= epochState1 ^. prevPParamsEpochStateL . ppProtocolVersionL
      then trans @(EraRule "HARDFORK" era) $ TRC ((), epochState1, curPv)
      else pure epochState1
  liftSTS $ setFreshDRepPulsingState eNo stakePoolDistr epochState2

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
  , EraCertState era
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

instance
  ( EraGov era
  , PredicateFailure (ConwayHARDFORK era) ~ Void
  , STS (ConwayHARDFORK era)
  , BaseM (ConwayHARDFORK era) ~ ShelleyBase
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  ) =>
  Embed (ConwayHARDFORK era) (ConwayEPOCH era)
  where
  wrapFailed = absurd
  wrapEvent = HardForkEvent

updateCommitteeState :: StrictMaybe (Committee era) -> CommitteeState era -> CommitteeState era
updateCommitteeState committee (CommitteeState creds) =
  CommitteeState $ Map.intersection creds members
  where
    members = foldMap' committeeMembers committee
