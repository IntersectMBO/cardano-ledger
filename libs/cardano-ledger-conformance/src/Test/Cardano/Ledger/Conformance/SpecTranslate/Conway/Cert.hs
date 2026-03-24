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
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
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
  ( SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map AccountAddress Coin)
  ) =>
  SpecTranslate ctx ConwayEra (CertEnv era)
  where
  type SpecRep ConwayEra (CertEnv era) = Agda.CertEnv
  toSpecRep CertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map AccountAddress Coin)
    let ccColdCreds = foldMap (keysSet . committeeMembers) ceCurrentCommittee
    Agda.MkCertEnv
      <$> toSpecRep @_ @ConwayEra ceCurrentEpoch
      <*> toSpecRep @_ @ConwayEra cePParams
      <*> toSpecRep @_ @ConwayEra votes
      <*> toSpecRep @_ @ConwayEra withdrawals
      <*> toSpecRep @_ @ConwayEra ccColdCreds

instance ConwayEraAccounts era => SpecTranslate ctx ConwayEra (ConwayCertState era) where
  type SpecRep ConwayEra (ConwayCertState era) = Agda.CertState
  toSpecRep ConwayCertState {..} =
    Agda.MkCertState
      <$> toSpecRep @_ @ConwayEra conwayCertDState
      <*> toSpecRep @_ @ConwayEra conwayCertPState
      <*> toSpecRep @_ @ConwayEra conwayCertVState

instance Era era => SpecTranslate ctx ConwayEra (ConwayTxCert era) where
  type SpecRep ConwayEra (ConwayTxCert era) = Agda.DCert

  toSpecRep (ConwayTxCertPool p) = toSpecRep @_ @ConwayEra p
  toSpecRep (ConwayTxCertGov c) = toSpecRep @_ @ConwayEra c
  toSpecRep (ConwayTxCertDeleg x) = toSpecRep @_ @ConwayEra x

depositsMap ::
  ConwayEraCertState era =>
  CertState era ->
  Proposals era ->
  SpecTransM ctx (Agda.HSMap Agda.DepositPurpose Integer)
depositsMap certState props =
  unionsHSMap
    <$> sequence
      [ bimapMHSMap
          (fmap Agda.CredentialDeposit . toSpecRep @_ @ConwayEra)
          (toSpecRep @_ @ConwayEra . fromCompact . view depositAccountStateL)
          (Agda.MkHSMap . Map.toList $ certState ^. certDStateL . accountsL . accountsMapL)
      , bimapMHSMap
          (fmap Agda.PoolDeposit . toSpecRep @_ @ConwayEra)
          toSpecRep
          (Agda.MkHSMap . Map.toList $ fromCompact . spsDeposit <$> certState ^. certPStateL . psStakePoolsL)
      , bimapMHSMap
          (fmap Agda.DRepDeposit . toSpecRep @_ @ConwayEra)
          (toSpecRep @_ @ConwayEra . drepDeposit)
          (Agda.MkHSMap . Map.toList $ certState ^. certVStateL . vsDRepsL)
      , bimapMHSMap
          (fmap Agda.GovActionDeposit . toSpecRep @_ @ConwayEra)
          (toSpecRep @_ @ConwayEra . gasDeposit)
          (Agda.MkHSMap . OMap.assocList $ props ^. pPropsL)
      ]

instance
  ( SpecTranslate ctx ConwayEra (TxOut era)
  , SpecRep ConwayEra (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  , Inject ctx (CertState era)
  , ConwayEraCertState era
  ) =>
  SpecTranslate ctx ConwayEra (UTxOState era)
  where
  type SpecRep ConwayEra (UTxOState era) = Agda.UTxOState

  toSpecRep UTxOState {..} = do
    certState <- askCtx @(CertState era)
    let
      props = utxosGovState ^. cgsProposalsL
      deposits = depositsMap certState props
    Agda.MkUTxOState
      <$> toSpecRep @_ @ConwayEra utxosUtxo
      <*> toSpecRep @_ @ConwayEra utxosFees
      <*> deposits
      <*> toSpecRep @_ @ConwayEra utxosDonation

instance
  ( ConwayEraGov era
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecRep ConwayEra (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  , SpecTranslate (CertState era) ConwayEra (TxOut era)
  , SpecRep ConwayEra (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate ctx ConwayEra (LedgerState era)
  where
  type SpecRep ConwayEra (LedgerState era) = Agda.LState

  toSpecRep (LedgerState {..}) = do
    let
      props = utxosGovState lsUTxOState ^. proposalsGovStateL
      deposits = depositsMap lsCertState props
    Agda.MkLState
      <$> withCtx lsCertState (toSpecRep @_ @ConwayEra lsUTxOState)
      <*> toSpecRep @_ @ConwayEra props
      <*> withCtx deposits (toSpecRep @_ @ConwayEra lsCertState)

instance
  ( EraPParams era
  , ConwayEraGov era
  , SpecTranslate [GovActionState era] ConwayEra (PParamsHKD Identity era)
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate [GovActionState era] ConwayEra (PParamsHKD StrictMaybe era)
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  , SpecTranslate (CertState era) ConwayEra (TxOut era)
  , SpecRep ConwayEra (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate ctx ConwayEra (EpochState era)
  where
  type SpecRep ConwayEra (EpochState era) = Agda.EpochState

  toSpecRep (EpochState {esLState = esLState@LedgerState {lsUTxOState}, ..}) =
    Agda.MkEpochState
      <$> toSpecRep @_ @ConwayEra esChainAccountState
      <*> toSpecRep @_ @ConwayEra esSnapshots
      <*> toSpecRep @_ @ConwayEra esLState
      <*> toSpecRep @_ @ConwayEra enactState
      <*> withCtx govActions (toSpecRep @_ @ConwayEra ratifyState)
    where
      enactState = mkEnactState $ utxosGovState lsUTxOState
      ratifyState = getRatifyState $ utxosGovState lsUTxOState
      govActions = toList $ lsUTxOState ^. utxosGovStateL . proposalsGovStateL . pPropsL

instance SpecTranslate ctx ConwayEra SnapShots where
  type SpecRep ConwayEra SnapShots = Agda.Snapshots

  toSpecRep (SnapShots {..}) =
    Agda.MkSnapshots
      <$> toSpecRep @_ @ConwayEra ssStakeMark
      <*> toSpecRep @_ @ConwayEra ssStakeSet
      <*> toSpecRep @_ @ConwayEra ssStakeGo
      <*> toSpecRep @_ @ConwayEra ssFee

instance SpecTranslate ctx ConwayEra SnapShot where
  type SpecRep ConwayEra SnapShot = Agda.Snapshot

  toSpecRep (SnapShot {..}) =
    Agda.MkSnapshot
      <$> toSpecRep @_ @ConwayEra (Stake $ VMap.fromMap $ Map.map (unNonZero . swdStake) activeStakeMap)
      <*> toSpecRep @_ @ConwayEra (Map.map swdDelegation activeStakeMap)
      <*> toSpecRep @_ @ConwayEra (VMap.toMap ssStakePoolsSnapShot)
    where
      activeStakeMap = VMap.toMap $ unActiveStake ssActiveStake

instance SpecTranslate ctx ConwayEra StakePoolSnapShot where
  type SpecRep ConwayEra StakePoolSnapShot = Agda.StakePoolParams

  toSpecRep StakePoolSnapShot {..} =
    Agda.StakePoolParams
      <$> toSpecRep @_ @ConwayEra spssSelfDelegatedOwners
      <*> toSpecRep @_ @ConwayEra spssCost
      <*> toSpecRep @_ @ConwayEra spssMargin
      <*> toSpecRep @_ @ConwayEra spssPledge
      <*> toSpecRep @_ @ConwayEra (unAccountId spssAccountId)

instance SpecTranslate ctx ConwayEra Stake where
  type SpecRep ConwayEra Stake = Agda.HSMap Agda.Credential Agda.Coin

  toSpecRep (Stake stake) = toSpecRep @_ @ConwayEra $ VMap.toMap stake

instance SpecTranslate ctx ConwayEra ChainAccountState where
  type SpecRep ConwayEra ChainAccountState = Agda.Acnt

  toSpecRep (ChainAccountState {..}) =
    Agda.MkAcnt
      <$> toSpecRep @_ @ConwayEra casTreasury
      <*> toSpecRep @_ @ConwayEra casReserves

instance SpecTranslate ctx ConwayEra DeltaCoin where
  type SpecRep ConwayEra DeltaCoin = Integer

  toSpecRep (DeltaCoin x) = pure x

instance SpecTranslate ctx ConwayEra PulsingRewUpdate where
  type SpecRep ConwayEra PulsingRewUpdate = Agda.HsRewardUpdate

  toSpecRep x =
    Agda.MkRewardUpdate
      <$> toSpecRep @_ @ConwayEra deltaT
      <*> toSpecRep @_ @ConwayEra deltaR
      <*> toSpecRep @_ @ConwayEra deltaF
      <*> toSpecRep @_ @ConwayEra rwds
    where
      (RewardUpdate {..}, _) = runShelleyBase $ completeRupd x
      rwds = Set.foldMap rewardAmount <$> rs

instance
  ( EraPParams era
  , ConwayEraGov era
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecTranslate [GovActionState era] ConwayEra (PParamsHKD Identity era)
  , SpecTranslate [GovActionState era] ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  , SpecTranslate (CertState era) ConwayEra (TxOut era)
  , SpecRep ConwayEra (CertState era) ~ Agda.CertState
  , ConwayEraCertState era
  , CertState era ~ ConwayCertState era
  ) =>
  SpecTranslate ctx ConwayEra (NewEpochState era)
  where
  type SpecRep ConwayEra (NewEpochState era) = Agda.NewEpochState

  toSpecRep (NewEpochState {..}) =
    Agda.MkNewEpochState
      <$> toSpecRep @_ @ConwayEra nesEL
      <*> toSpecRep @_ @ConwayEra nesBprev
      <*> toSpecRep @_ @ConwayEra nesBcur
      <*> toSpecRep @_ @ConwayEra nesEs
      <*> toSpecRep @_ @ConwayEra nesRu
      <*> (filterZeroEntries <$> toSpecRep @_ @ConwayEra nesPd)
    where
      -- The specification does not include zero entries in general
      -- while the implementation might. So we filter them out here for the sake
      -- of comparing results.
      --
      -- The discrepancy is discussed here:
      -- https://github.com/IntersectMBO/cardano-ledger/issues/5306
      filterZeroEntries (Agda.MkHSMap lst) =
        Agda.MkHSMap $ filter ((/= 0) . snd) lst
