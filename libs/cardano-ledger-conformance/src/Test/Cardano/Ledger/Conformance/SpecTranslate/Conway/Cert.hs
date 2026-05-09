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

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Rewards (rewardAmount)
import Cardano.Ledger.Shelley.LedgerState
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as Set
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map, keysSet)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.VMap as VMap
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.GovCert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Conway.TreeDiff
import Test.Cardano.Ledger.Shelley.Utils (runShelleyBase)

instance
  ( SpecTranslate (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecContext (PParamsHKD Identity era) ~ ()
  ) =>
  SpecTranslate (Conway.CertEnv era)
  where
  type SpecRep (Conway.CertEnv era) = Agda.CertEnv
  type SpecContext (Conway.CertEnv era) = (VotingProcedures era, Map AccountAddress Coin)
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

instance ConwayEraAccounts era => SpecTranslate (ConwayCertState era) where
  type SpecRep (ConwayCertState era) = Agda.CertState
  toSpecRep ConwayCertState {..} =
    Agda.MkCertState
      <$> toSpecRep conwayCertDState
      <*> toSpecRep conwayCertPState
      <*> toSpecRep conwayCertVState

instance Era era => SpecTranslate (ConwayTxCert era) where
  type SpecRep (ConwayTxCert era) = Agda.DCert

  toSpecRep (ConwayTxCertPool p) = toSpecRep p
  toSpecRep (ConwayTxCertGov c) = toSpecRep c
  toSpecRep (ConwayTxCertDeleg x) = toSpecRep x

depositsMap ::
  ConwayEraCertState era =>
  CertState era ->
  Proposals era ->
  SpecTransM () (Agda.HSMap Agda.DepositPurpose Integer)
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

instance
  ( SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecContext (TxOut era) ~ ()
  , GovState era ~ ConwayGovState era
  , ConwayEraCertState era
  ) =>
  SpecTranslate (UTxOState era)
  where
  type SpecRep (UTxOState era) = Agda.UTxOState
  type SpecContext (UTxOState era) = CertState era

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

instance
  ( ConwayEraGov era
  , EraPParams era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecContext (TxOut era) ~ ()
  , GovState era ~ ConwayGovState era
  , SpecRep (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate (LedgerState era)
  where
  type SpecRep (LedgerState era) = Agda.LState

  toSpecRep (LedgerState {..}) =
    Agda.MkLState
      <$> withCtxSpecTransM lsCertState (toSpecRep lsUTxOState)
      <*> toSpecRep props
      <*> toSpecRep lsCertState
    where
      props = utxosGovState lsUTxOState ^. proposalsGovStateL

instance
  ( EraPParams era
  , ConwayEraGov era
  , SpecTranslate (PParamsHKD Identity era)
  , SpecContext (PParamsHKD Identity era) ~ ()
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecContext (TxOut era) ~ ()
  , GovState era ~ ConwayGovState era
  , SpecRep (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate (EpochState era)
  where
  type SpecRep (EpochState era) = Agda.EpochState

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

instance SpecTranslate SnapShots where
  type SpecRep SnapShots = Agda.Snapshots

  toSpecRep (SnapShots {..}) =
    Agda.MkSnapshots
      <$> toSpecRep ssStakeMark
      <*> toSpecRep ssStakeSet
      <*> toSpecRep ssStakeGo
      <*> toSpecRep ssFee

instance SpecTranslate SnapShot where
  type SpecRep SnapShot = Agda.Snapshot

  toSpecRep (SnapShot {..}) =
    Agda.MkSnapshot
      <$> toSpecRep (Stake $ VMap.fromMap $ Map.map (unNonZero . swdStake) activeStakeMap)
      <*> toSpecRepMap (Map.map swdDelegation activeStakeMap)
      <*> toSpecRepMap (VMap.toMap ssStakePoolsSnapShot)
    where
      activeStakeMap = VMap.toMap $ unActiveStake ssActiveStake

instance SpecTranslate StakePoolSnapShot where
  type SpecRep StakePoolSnapShot = Agda.StakePoolParams

  toSpecRep StakePoolSnapShot {..} =
    Agda.StakePoolParams
      <$> toSpecRep spssSelfDelegatedOwners
      <*> toSpecRep spssCost
      <*> toSpecRep spssMargin
      <*> toSpecRep spssPledge
      <*> toSpecRep (unAccountId spssAccountId)

instance SpecTranslate Stake where
  type SpecRep Stake = Agda.HSMap Agda.Credential Agda.Coin

  toSpecRep (Stake stake) = toSpecRepMap $ VMap.toMap stake

instance SpecTranslate ChainAccountState where
  type SpecRep ChainAccountState = Agda.Acnt

  toSpecRep (ChainAccountState {..}) =
    Agda.MkAcnt
      <$> toSpecRep casTreasury
      <*> toSpecRep casReserves

instance SpecTranslate DeltaCoin where
  type SpecRep DeltaCoin = Integer

  toSpecRep (DeltaCoin x) = pure x

instance SpecTranslate PulsingRewUpdate where
  type SpecRep PulsingRewUpdate = Agda.HsRewardUpdate

  toSpecRep x =
    Agda.MkRewardUpdate
      <$> toSpecRep deltaT
      <*> toSpecRep deltaR
      <*> toSpecRep deltaF
      <*> toSpecRepMap rwds
    where
      (RewardUpdate {..}, _) = runShelleyBase $ completeRupd x
      rwds = Set.foldMap rewardAmount <$> rs

instance
  ( EraPParams era
  , ConwayEraGov era
  , SpecTranslate (PParamsHKD Identity era)
  , SpecContext (PParamsHKD Identity era) ~ ()
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecContext (TxOut era) ~ ()
  , GovState era ~ ConwayGovState era
  , SpecRep (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate (NewEpochState era)
  where
  type SpecRep (NewEpochState era) = Agda.NewEpochState

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
