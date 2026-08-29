{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Epoch () where

import Cardano.Crypto.Util (bytesToNatural)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (rawEncodeFixedSized)
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core (ppLeiosCommitteeSizeG)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Rewards (rewardAmount)
import Cardano.Ledger.Shelley.LedgerState
import Data.Foldable (Foldable (..))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.VMap as VMap
import Lens.Micro
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  toSpecRepMap,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.GovCert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Ledger ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Pool ()
import Test.Cardano.Ledger.Shelley.Utils (runShelleyBase)

instance SpecTranslate DijkstraEra (EpochState DijkstraEra) where
  type SpecRep DijkstraEra (EpochState DijkstraEra) = Agda.EpochState

  type SpecContext DijkstraEra (EpochState DijkstraEra) = Network

  toSpecRep (EpochState {esLState = esLState@LedgerState {lsUTxOState}, ..}) = do
    netId <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkEpochState
        <$> toSpecRep esChainAccountState
        <*> toSpecRep esSnapshots
        <*> withCtxSpecTransM netId (toSpecRep esLState)
        <*> toSpecRep enactState
        <*> withCtxSpecTransM govActions (toSpecRep ratifyState)
    where
      enactState = mkEnactState $ utxosGovState lsUTxOState
      ratifyState = getRatifyState $ utxosGovState lsUTxOState
      govActions = toList $ lsUTxOState ^. utxosGovStateL . proposalsGovStateL . pPropsL

instance SpecTranslate DijkstraEra (SnapShots DijkstraEra) where
  type SpecRep DijkstraEra (SnapShots DijkstraEra) = Agda.Snapshots

  toSpecRep (SnapShots {..}) =
    Agda.MkSnapshots
      <$> toSpecRep ssStakeMark
      <*> toSpecRep ssStakeSet
      <*> toSpecRep ssStakeGo
      <*> toSpecRep ssFee

instance SpecTranslate DijkstraEra SnapShot where
  type SpecRep DijkstraEra SnapShot = Agda.Snapshot

  toSpecRep (SnapShot {..}) =
    Agda.MkSnapshot
      <$> toSpecRep (Stake $ VMap.fromMap $ Map.map (unNonZero . swdStake) activeStakeMap)
      <*> toSpecRepMap (Map.map swdDelegation activeStakeMap)
      <*> toSpecRepMap (VMap.toMap ssStakePoolsSnapShot)
    where
      activeStakeMap = VMap.toMap $ unActiveStake ssActiveStake

instance SpecTranslate DijkstraEra StakePoolSnapShot where
  type SpecRep DijkstraEra StakePoolSnapShot = Agda.StakePoolState

  toSpecRep StakePoolSnapShot {..} =
    Agda.StakePoolState
      <$> ( Agda.StakePoolParams
              <$> toSpecRep spssSelfDelegatedOwners
              <*> toSpecRep spssCost
              <*> toSpecRep spssMargin
              <*> toSpecRep spssPledge
              <*> (Agda.RewardAddress <$> pure 0 <*> toSpecRep (unAccountId spssAccountId))
              <*> toSpecRep spssVrf
          )
      <*> traverse toSpecRep (strictMaybeToMaybe spssBlsKey)

instance SpecTranslate DijkstraEra Stake where
  type SpecRep DijkstraEra Stake = Agda.HSMap Agda.Credential Agda.Coin

  toSpecRep (Stake stake) = toSpecRepMap $ VMap.toMap stake

instance SpecTranslate DijkstraEra ChainAccountState where
  type SpecRep DijkstraEra ChainAccountState = Agda.Acnt

  toSpecRep (ChainAccountState {..}) =
    Agda.MkAcnt
      <$> toSpecRep casTreasury
      <*> toSpecRep casReserves

instance SpecTranslate DijkstraEra DeltaCoin where
  type SpecRep DijkstraEra DeltaCoin = Integer

  toSpecRep (DeltaCoin x) = pure x

instance SpecTranslate DijkstraEra PulsingRewUpdate where
  type SpecRep DijkstraEra PulsingRewUpdate = Agda.RewardUpdate

  toSpecRep x =
    Agda.MkRewardUpdate
      <$> toSpecRep deltaT
      <*> toSpecRep deltaR
      <*> toSpecRep deltaF
      <*> toSpecRepMap rwds
    where
      (RewardUpdate {..}, _) = runShelleyBase $ completeRupd x
      rwds = foldMap rewardAmount <$> rs

instance SpecTranslate DijkstraEra (NewEpochState DijkstraEra) where
  type SpecRep DijkstraEra (NewEpochState DijkstraEra) = Agda.NewEpochState

  type SpecContext DijkstraEra (NewEpochState DijkstraEra) = Network
  toSpecRep (NewEpochState {..}) = do
    netId <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkNewEpochState
        <$> toSpecRep nesEL
        <*> toSpecRep nesBprev
        <*> toSpecRep nesBcur
        <*> withCtxSpecTransM netId (toSpecRep nesEs)
        <*> toSpecRep nesRu
        <*> (filterZeroEntries <$> toSpecRep nesPd)
        <*> committee
    where
      filterZeroEntries (Agda.MkHSMap lst) =
        Agda.MkHSMap $ filter ((/= 0) . snd) lst
      -- The ledger derives the committee from the pool distribution rather than storing it,
      -- so the selection the spec materializes into 'nesLeiosCommittee' happens here: the
      -- 'leiosCommitteeSize' pools with most stake, ties broken by ascending pool id.
      committee =
        Agda.MkHSMap . zip [0 ..] <$> traverse toSpecSeat (take committeeSize orderedPools)
      committeeSize = fromIntegral $ nesEs ^. curPParamsEpochStateL . ppLeiosCommitteeSizeG
      orderedPools =
        sortOn (\(poolId, ips) -> (Down (individualTotalPoolStake ips), poolId))
          . filter ((/= mempty) . individualTotalPoolStake . snd)
          . Map.toList
          $ unPoolDistr nesPd
      toSpecSeat (poolId, ips) =
        Agda.MkLeiosSeat
          <$> toSpecRep poolId
          <*> toSpecRep (fromCompact (individualTotalPoolStake ips))
          <*> pure (blsVKey <$> strictMaybeToMaybe (individualPoolStakeBls ips))
      blsVKey = toInteger . bytesToNatural . rawEncodeFixedSized . blsPubKey
