{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert () where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Shelley.LedgerState
import qualified Data.Foldable as Set
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map, keysSet)
import qualified Data.VMap as VMap
import Lens.Micro
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.GovCert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Constrained.Conway.Utxo (depositsMap)
import Test.Cardano.Ledger.Conway.TreeDiff
import Test.Cardano.Ledger.Shelley.Utils (runShelleyBase)

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map RewardAccount Coin)
  ) =>
  SpecTranslate ctx (CertEnv era)
  where
  type SpecRep (CertEnv era) = Agda.CertEnv
  toSpecRep CertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map RewardAccount Coin)
    let ccColdCreds = foldMap (keysSet . committeeMembers) ceCurrentCommittee
    Agda.MkCertEnv
      <$> toSpecRep ceCurrentEpoch
      <*> toSpecRep cePParams
      <*> toSpecRep votes
      <*> toSpecRep withdrawals
      <*> toSpecRep ccColdCreds

instance ConwayEraAccounts era => SpecTranslate ctx (ConwayCertState era) where
  type SpecRep (ConwayCertState era) = Agda.CertState
  toSpecRep ConwayCertState {..} =
    Agda.MkCertState
      <$> toSpecRep conwayCertDState
      <*> toSpecRep conwayCertPState
      <*> toSpecRep conwayCertVState

instance Era era => SpecTranslate ctx (ConwayTxCert era) where
  type SpecRep (ConwayTxCert era) = Agda.DCert

  toSpecRep (ConwayTxCertPool p) = toSpecRep p
  toSpecRep (ConwayTxCertGov c) = toSpecRep c
  toSpecRep (ConwayTxCertDeleg x) = toSpecRep x

instance
  ( SpecTranslate ctx (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  , Inject ctx (CertState era)
  , ConwayEraCertState era
  ) =>
  SpecTranslate ctx (UTxOState era)
  where
  type SpecRep (UTxOState era) = Agda.UTxOState

  toSpecRep UTxOState {..} = do
    certState <- askCtx @(CertState era)
    let
      props = utxosGovState ^. cgsProposalsL
      deposits = depositsMap certState props
    Agda.MkUTxOState
      <$> toSpecRep utxosUtxo
      <*> toSpecRep utxosFees
      <*> toSpecRep deposits
      <*> toSpecRep utxosDonation

instance
  ( ConwayEraGov era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecRep (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  , SpecTranslate (CertState era) (TxOut era)
  , SpecRep (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate ctx (LedgerState era)
  where
  type SpecRep (LedgerState era) = Agda.LState

  toSpecRep (LedgerState {..}) = do
    let
      props = utxosGovState lsUTxOState ^. proposalsGovStateL
      deposits = depositsMap lsCertState props
    Agda.MkLState
      <$> withCtx lsCertState (toSpecRep lsUTxOState)
      <*> toSpecRep props
      <*> withCtx deposits (toSpecRep lsCertState)

instance
  ( EraPParams era
  , ConwayEraGov era
  , SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , Inject ctx [GovActionState era]
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  , SpecTranslate (CertState era) (TxOut era)
  , SpecRep (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate ctx (EpochState era)
  where
  type SpecRep (EpochState era) = Agda.EpochState

  toSpecRep (EpochState {esLState = esLState@LedgerState {lsUTxOState}, ..}) =
    Agda.MkEpochState
      <$> toSpecRep esChainAccountState
      <*> toSpecRep esSnapshots
      <*> toSpecRep esLState
      <*> toSpecRep enactState
      <*> toSpecRep ratifyState
    where
      enactState = mkEnactState $ utxosGovState lsUTxOState
      ratifyState = RatifyState enactState mempty mempty False

instance SpecTranslate ctx SnapShots where
  type SpecRep SnapShots = Agda.Snapshots

  toSpecRep (SnapShots {..}) =
    Agda.MkSnapshots
      <$> toSpecRep ssStakeMark
      <*> toSpecRep ssStakeSet
      <*> toSpecRep ssStakeGo
      <*> toSpecRep ssFee

instance SpecTranslate ctx SnapShot where
  type SpecRep SnapShot = Agda.Snapshot

  toSpecRep (SnapShot {..}) =
    Agda.MkSnapshot
      <$> toSpecRep ssStake
      <*> toSpecRep (VMap.toMap ssDelegations)

instance SpecTranslate ctx Stake where
  type SpecRep Stake = Agda.HSMap Agda.Credential Agda.Coin

  toSpecRep (Stake stake) = toSpecRep $ VMap.toMap stake

instance SpecTranslate ctx ChainAccountState where
  type SpecRep ChainAccountState = Agda.Acnt

  toSpecRep (ChainAccountState {..}) =
    Agda.MkAcnt
      <$> toSpecRep casTreasury
      <*> toSpecRep casReserves

instance SpecTranslate ctx DeltaCoin where
  type SpecRep DeltaCoin = Integer

  toSpecRep (DeltaCoin x) = pure x

instance SpecTranslate ctx PulsingRewUpdate where
  type SpecRep PulsingRewUpdate = Agda.HsRewardUpdate

  toSpecRep x =
    Agda.MkRewardUpdate
      <$> toSpecRep deltaT
      <*> toSpecRep deltaR
      <*> toSpecRep deltaF
      <*> toSpecRep rwds
    where
      (RewardUpdate {..}, _) = runShelleyBase $ completeRupd x
      rwds = Set.foldMap rewardAmount <$> rs

instance
  ( EraPParams era
  , ConwayEraGov era
  , SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , Inject ctx [GovActionState era]
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  , SpecTranslate (CertState era) (TxOut era)
  , SpecRep (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate ctx (NewEpochState era)
  where
  type SpecRep (NewEpochState era) = Agda.NewEpochState

  toSpecRep (NewEpochState {..}) =
    Agda.MkNewEpochState
      <$> toSpecRep nesEL
      <*> toSpecRep nesEs
      <*> toSpecRep nesRu
