{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Rewards (rewardAmount)
import Cardano.Ledger.Shelley.LedgerState
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as Set
import Data.Map.Strict (Map, keysSet)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.VMap as VMap
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTransM,
  SpecTranslate (..),
  askSpecTransM,
  toSpecRepMap,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.GovCert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Conformance.Utils (
  bimapMHSMap,
  unionsHSMap,
 )
import Test.Cardano.Ledger.Shelley.Utils (runShelleyBase)

instance SpecTranslate ConwayEra (Conway.CertEnv ConwayEra) where
  type SpecRep ConwayEra (Conway.CertEnv ConwayEra) = Agda.CertEnv
  type
    SpecContext ConwayEra (Conway.CertEnv ConwayEra) =
      (VotingProcedures ConwayEra, Map AccountAddress Coin)
  toSpecRep Conway.CertEnv {..} = do
    (votes, withdrawals) <- askSpecTransM
    let ccColdCreds = foldMap (keysSet . committeeMembers) ceCurrentCommittee
    withCtxSpecTransM () $
      Agda.MkCertEnv
        <$> toSpecRep ceCurrentEpoch
        <*> toSpecRep cePParams
        <*> toSpecRep votes
        <*> toSpecRepMap withdrawals
        <*> toSpecRep ccColdCreds

instance SpecTranslate ConwayEra (ConwayCertState ConwayEra) where
  type SpecRep ConwayEra (ConwayCertState ConwayEra) = Agda.CertState
  toSpecRep ConwayCertState {..} =
    Agda.MkCertState
      <$> toSpecRep conwayCertDState
      <*> toSpecRep conwayCertPState
      <*> toSpecRep conwayCertVState

instance SpecTranslate ConwayEra (ConwayTxCert ConwayEra) where
  type SpecRep ConwayEra (ConwayTxCert ConwayEra) = Agda.DCert

  toSpecRep (ConwayTxCertPool p) = toSpecRep p
  toSpecRep (ConwayTxCertGov c) = toSpecRep c
  toSpecRep (ConwayTxCertDeleg x) = toSpecRep x

depositsMap ::
  CertState ConwayEra ->
  Proposals ConwayEra ->
  SpecTransM ConwayEra () (Agda.HSMap Agda.DepositPurpose Integer)
depositsMap certState props =
  unionsHSMap
    <$> sequence
      [ bimapMHSMap
          (fmap Agda.CredentialDeposit . toSpecRep)
          (toSpecRep . fromCompact . view depositAccountStateL)
          (Agda.MkHSMap . Map.toList $ certState ^. certDStateL . accountsL . accountsMapL)
      , bimapMHSMap
          (fmap Agda.PoolDeposit . toSpecRep)
          toSpecRep
          (Agda.MkHSMap . Map.toList $ fromCompact . spsDeposit <$> certState ^. certPStateL . psStakePoolsL)
      , bimapMHSMap
          (fmap Agda.DRepDeposit . toSpecRep)
          (toSpecRep . drepDeposit)
          (Agda.MkHSMap . Map.toList $ certState ^. certVStateL . vsDRepsL)
      , bimapMHSMap
          (fmap Agda.GovActionDeposit . toSpecRep)
          (toSpecRep . gasDeposit)
          (Agda.MkHSMap . OMap.assocList $ props ^. pPropsL)
      ]

instance SpecTranslate ConwayEra (UTxOState ConwayEra) where
  type SpecRep ConwayEra (UTxOState ConwayEra) = Agda.UTxOState
  type SpecContext ConwayEra (UTxOState ConwayEra) = CertState ConwayEra

  toSpecRep UTxOState {..} = do
    certState <- askSpecTransM
    let
      props = utxosGovState ^. cgsProposalsL
      deposits = depositsMap certState props
    withCtxSpecTransM () $
      Agda.MkUTxOState
        <$> toSpecRep utxosUtxo
        <*> toSpecRep utxosFees
        <*> deposits
        <*> toSpecRep utxosDonation

instance SpecTranslate ConwayEra (LedgerState ConwayEra) where
  type SpecRep ConwayEra (LedgerState ConwayEra) = Agda.LState

  toSpecRep (LedgerState {..}) =
    Agda.MkLState
      <$> withCtxSpecTransM lsCertState (toSpecRep lsUTxOState)
      <*> toSpecRep props
      <*> toSpecRep lsCertState
    where
      props = utxosGovState lsUTxOState ^. proposalsGovStateL

instance SpecTranslate ConwayEra (EpochState ConwayEra) where
  type SpecRep ConwayEra (EpochState ConwayEra) = Agda.EpochState

  toSpecRep (EpochState {esLState = esLState@LedgerState {lsUTxOState}, ..}) =
    Agda.MkEpochState
      <$> toSpecRep esChainAccountState
      <*> toSpecRep esSnapshots
      <*> toSpecRep esLState
      <*> toSpecRep enactState
      <*> withCtxSpecTransM govActions (toSpecRep ratifyState)
    where
      enactState = mkEnactState $ utxosGovState lsUTxOState
      ratifyState = getRatifyState $ utxosGovState lsUTxOState
      govActions = toList $ lsUTxOState ^. utxosGovStateL . proposalsGovStateL . pPropsL

instance SpecTranslate ConwayEra (SnapShots era) where
  type SpecRep ConwayEra (SnapShots era) = Agda.Snapshots

  toSpecRep (SnapShots {..}) =
    Agda.MkSnapshots
      <$> toSpecRep ssStakeMark
      <*> toSpecRep ssStakeSet
      <*> toSpecRep ssStakeGo
      <*> toSpecRep ssFee

instance SpecTranslate ConwayEra SnapShot where
  type SpecRep ConwayEra SnapShot = Agda.Snapshot

  toSpecRep (SnapShot {..}) =
    Agda.MkSnapshot
      <$> toSpecRep (Stake $ VMap.fromMap $ Map.map (unNonZero . swdStake) activeStakeMap)
      <*> toSpecRepMap (Map.map swdDelegation activeStakeMap)
      <*> toSpecRepMap (VMap.toMap ssStakePoolsSnapShot)
    where
      activeStakeMap = VMap.toMap $ unActiveStake ssActiveStake

instance SpecTranslate ConwayEra StakePoolSnapShot where
  type SpecRep ConwayEra StakePoolSnapShot = Agda.StakePoolParams

  toSpecRep StakePoolSnapShot {..} =
    Agda.StakePoolParams
      <$> toSpecRep spssSelfDelegatedOwners
      <*> toSpecRep spssCost
      <*> toSpecRep spssMargin
      <*> toSpecRep spssPledge
      <*> toSpecRep (unAccountId spssAccountId)

instance SpecTranslate ConwayEra Stake where
  type SpecRep ConwayEra Stake = Agda.HSMap Agda.Credential Agda.Coin

  toSpecRep (Stake stake) = toSpecRepMap $ VMap.toMap stake

instance SpecTranslate ConwayEra ChainAccountState where
  type SpecRep ConwayEra ChainAccountState = Agda.Acnt

  toSpecRep (ChainAccountState {..}) =
    Agda.MkAcnt
      <$> toSpecRep casTreasury
      <*> toSpecRep casReserves

instance SpecTranslate ConwayEra DeltaCoin where
  type SpecRep ConwayEra DeltaCoin = Integer

  toSpecRep (DeltaCoin x) = pure x

instance SpecTranslate ConwayEra PulsingRewUpdate where
  type SpecRep ConwayEra PulsingRewUpdate = Agda.HsRewardUpdate

  toSpecRep x =
    Agda.MkRewardUpdate
      <$> toSpecRep deltaT
      <*> toSpecRep deltaR
      <*> toSpecRep deltaF
      <*> toSpecRepMap rwds
    where
      (RewardUpdate {..}, _) = runShelleyBase $ completeRupd x
      rwds = Set.foldMap rewardAmount <$> rs

instance SpecTranslate ConwayEra (NewEpochState ConwayEra) where
  type SpecRep ConwayEra (NewEpochState ConwayEra) = Agda.NewEpochState

  toSpecRep (NewEpochState {..}) =
    Agda.MkNewEpochState
      <$> toSpecRep nesEL
      <*> toSpecRep nesBprev
      <*> toSpecRep nesBcur
      <*> toSpecRep nesEs
      <*> toSpecRep nesRu
      <*> (filterZeroEntries <$> toSpecRep nesPd)
    where
      -- The specification does not include zero entries in general
      -- while the implementation might. So we filter them out here for the sake
      -- of comparing results.
      --
      -- The discrepancy is discussed here:
      -- https://github.com/IntersectMBO/cardano-ledger/issues/5306
      filterZeroEntries (Agda.MkHSMap lst) =
        Agda.MkHSMap $ filter ((/= 0) . snd) lst
