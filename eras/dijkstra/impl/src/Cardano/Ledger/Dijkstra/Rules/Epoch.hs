{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Epoch () where

import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState,
  EnactState (..),
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
  epochStateDRepPulsingStateL,
  extractDRepPulsingState,
  proposalsApplyEnactment,
  proposalsGovStateL,
  setFreshDRepPulsingState,
 )
import Cardano.Ledger.Conway.Rules (
  ConwayEpochEvent (..),
  ConwayHardForkEvent,
  ConwayNewEpochEvent (EpochEvent),
  HARDFORK,
  NEWEPOCH,
  RATIFY,
  applyEnactedWithdrawals,
  returnProposalDeposits,
  updateCommitteeState,
  updateNumDormantEpochs,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (EPOCH, SNAP)
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams)
import Cardano.Ledger.Dijkstra.Rules.Snap ()
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  UTxOState (..),
  curPParamsEpochStateL,
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
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Slot (EpochNo)
import Cardano.Ledger.Val (zero)
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
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Lens.Micro ((%~), (&), (.~), (<>~), (^.))

instance
  ( EraTxOut era
  , RunConwayRatify era
  , ConwayEraCertState era
  , ConwayEraGov era
  , EraStake era
  , EraCertState era
  , Embed (EraRule "SNAP" era) (EPOCH era)
  , Environment (EraRule "SNAP" era) ~ Shelley.SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots era
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (EPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ ()
  , State (EraRule "POOLREAP" era) ~ Shelley.ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "RATIFY" era) (EPOCH era)
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , GovState era ~ ConwayGovState era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  , Embed (EraRule "HARDFORK" era) (EPOCH era)
  , Environment (EraRule "HARDFORK" era) ~ ()
  , State (EraRule "HARDFORK" era) ~ EpochState era
  , Signal (EraRule "HARDFORK" era) ~ ProtVer
  ) =>
  STS (EPOCH era)
  where
  type State (EPOCH era) = EpochState era
  type Signal (EPOCH era) = EpochNo
  type Environment (EPOCH era) = ()
  type BaseM (EPOCH era) = ShelleyBase
  type PredicateFailure (EPOCH era) = Void
  type Event (EPOCH era) = ConwayEpochEvent era
  transitionRules = [epochTransition]

epochTransition ::
  forall era.
  ( RunConwayRatify era
  , ConwayEraCertState era
  , EraTxOut era
  , Environment (EraRule "SNAP" era) ~ Shelley.SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots era
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "SNAP" era) (EPOCH era)
  , Embed (EraRule "POOLREAP" era) (EPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ ()
  , State (EraRule "POOLREAP" era) ~ Shelley.ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "RATIFY" era) (EPOCH era)
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , GovState era ~ ConwayGovState era
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  , ConwayEraGov era
  , Embed (EraRule "HARDFORK" era) (EPOCH era)
  , Environment (EraRule "HARDFORK" era) ~ ()
  , State (EraRule "HARDFORK" era) ~ EpochState era
  , Signal (EraRule "HARDFORK" era) ~ ProtVer
  ) =>
  TransitionRule (EPOCH era)
epochTransition = do
  TRC
    ( ()
      , epochState0@EpochState
          { esSnapshots = snapshots0
          , esLState = ledgerState0
          }
      , eNo
      ) <-
    judgmentContext
  let chainAccountState0 = epochState0 ^. chainAccountStateL
      govState0 = utxosGovState utxoState0
      curPParams = govState0 ^. curPParamsGovStateL
      utxoState0 = lsUTxOState ledgerState0
      certState0 = ledgerState0 ^. lsCertStateL
      vState = certState0 ^. certVStateL
  Shelley.PoolreapState utxoState1 chainAccountState1 certState1 <-
    trans @(EraRule "POOLREAP" era) $
      TRC ((), Shelley.PoolreapState utxoState0 chainAccountState0 certState0, eNo)

  let
    pulsingState = epochState0 ^. epochStateDRepPulsingStateL

    ratifyState@RatifyState {rsEnactState, rsEnacted, rsExpired} =
      extractDRepPulsingState pulsingState

    (chainAccountState2, dState2, EnactState {..}) =
      applyEnactedWithdrawals chainAccountState1 (certState1 ^. certDStateL) rsEnactState

    (newProposals, enactedActions, removedDueToEnactment, expiredActions) =
      proposalsApplyEnactment rsEnacted rsExpired (govState0 ^. proposalsGovStateL)

    govState1 =
      govState0
        & cgsProposalsL .~ newProposals
        & cgsCommitteeL .~ ensCommittee
        & cgsConstitutionL .~ ensConstitution
        & cgsCurPParamsL .~ nextEpochPParams govState0
        & cgsPrevPParamsL .~ curPParams
        & cgsFuturePParamsL .~ PotentialPParamsUpdate Nothing

    allRemovedGovActions = Map.unions [expiredActions, enactedActions, removedDueToEnactment]
    (newAccounts, unclaimed) =
      returnProposalDeposits allRemovedGovActions $ dState2 ^. accountsL
  tellEvent $
    GovInfoEvent
      (Set.fromList $ Map.elems enactedActions)
      (Set.fromList $ Map.elems removedDueToEnactment)
      (Set.fromList $ Map.elems expiredActions)
      unclaimed

  let
    certState2 =
      mkConwayCertState
        ( updateNumDormantEpochs eNo newProposals vState
            & vsCommitteeStateL %~ updateCommitteeState (govState1 ^. cgsCommitteeL)
        )
        (certState1 ^. certPStateL)
        (dState2 & accountsL .~ newAccounts)
    chainAccountState3 =
      chainAccountState2
        & casTreasuryL <>~ (utxoState0 ^. utxosDonationL <> fold unclaimed)
    utxoState2 =
      utxoState1
        & utxosDepositedL .~ totalObligation certState2 govState1
        & utxosDonationL .~ zero
        & utxosGovStateL .~ govState1
    ledgerState1 =
      ledgerState0
        & lsCertStateL .~ certState2
        & lsUTxOStateL .~ utxoState2
    epochState1 =
      epochState0
        & chainAccountStateL .~ chainAccountState3
        & esLStateL .~ ledgerState1
  tellEvent $ EpochBoundaryRatifyState ratifyState
  epochState2 <- do
    let curPv = epochState1 ^. curPParamsEpochStateL . ppProtocolVersionL
    if curPv /= epochState1 ^. prevPParamsEpochStateL . ppProtocolVersionL
      then trans @(EraRule "HARDFORK" era) $ TRC ((), epochState1, curPv)
      else pure epochState1
  snapshots1 <-
    trans @(EraRule "SNAP" era) $
      TRC
        ( Shelley.SnapEnv (epochState2 ^. esLStateL) (epochState2 ^. curPParamsEpochStateL)
        , snapshots0
        , ()
        )
  let
    stakePoolDistr = ssStakeMarkPoolDistr snapshots1
    epochState3 = epochState2 & esSnapshotsL .~ snapshots1
  liftSTS $ setFreshDRepPulsingState eNo stakePoolDistr epochState3

instance
  ( Era era
  , STS (Shelley.POOLREAP era)
  , Event (EraRule "POOLREAP" era) ~ Shelley.ShelleyPoolreapEvent era
  ) =>
  Embed (Shelley.POOLREAP era) (EPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = PoolReapEvent

instance
  ( EraTxOut era
  , EraStake era
  , EraCertState era
  , DijkstraEraPParams era
  , Event (EraRule "SNAP" era) ~ Shelley.SnapEvent era
  ) =>
  Embed (SNAP era) (EPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = SnapEvent

instance
  ( EraGov era
  , PredicateFailure (RATIFY era) ~ Void
  , STS (RATIFY era)
  , BaseM (RATIFY era) ~ ShelleyBase
  , Event (RATIFY era) ~ Void
  ) =>
  Embed (RATIFY era) (EPOCH era)
  where
  wrapFailed = absurd
  wrapEvent = absurd

instance
  ( EraGov era
  , PredicateFailure (HARDFORK era) ~ Void
  , STS (HARDFORK era)
  , BaseM (HARDFORK era) ~ ShelleyBase
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  ) =>
  Embed (HARDFORK era) (EPOCH era)
  where
  wrapFailed = absurd
  wrapEvent = HardForkEvent

instance
  ( STS (EPOCH era)
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  ) =>
  Embed (EPOCH era) (NEWEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = EpochEvent
