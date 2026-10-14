{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Epoch () where

import Cardano.Crypto.Leios
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Rewards (rewardAmount)
import Cardano.Ledger.Shelley.LedgerState
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import qualified Data.Vector.Strict as V
import Lens.Micro
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  toSpecRepMap,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (verkeyToInteger)
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
      <$> toSpecRep (msSnapShot ssStakeMark)
      <*> toSpecRep (ssSnapShot ssStakeSet)
      <*> toSpecRep (gsSnapShot ssStakeGo)
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
      <$> toSpecRep spssSelfDelegatedOwners
      <*> toSpecRep spssCost
      <*> toSpecRep spssMargin
      <*> toSpecRep spssPledge
      <*> (Agda.RewardAddress <$> pure 0 <*> toSpecRep (unAccountId spssAccountId))
      <*> toSpecRep spssVrf
      <*> toSpecRep spssBlsKey

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

instance SpecTranslate DijkstraEra Weight where
  type SpecRep DijkstraEra Weight = Agda.Rational

  toSpecRep = pure

instance SpecTranslate DijkstraEra LeiosVerificationKey where
  type SpecRep DijkstraEra LeiosVerificationKey = Integer

  toSpecRep = pure . verkeyToInteger

instance SpecTranslate DijkstraEra LeiosSeat where
  type SpecRep DijkstraEra LeiosSeat = Agda.LeiosSeat

  toSpecRep (LeiosSeat {..}) = Agda.MkLeiosSeat 0 <$> toSpecRep seatWeight <*> toSpecRep seatVKey

instance SpecTranslate DijkstraEra LeiosCommittee where
  type SpecRep DijkstraEra LeiosCommittee = [Agda.LeiosSeat]

  toSpecRep = mapM toSpecRep . V.toList . leiosCommitteeSeats

instance SpecTranslate DijkstraEra (NewEpochState DijkstraEra) where
  type SpecRep DijkstraEra (NewEpochState DijkstraEra) = Agda.NewEpochState

  type SpecContext DijkstraEra (NewEpochState DijkstraEra) = Network
  toSpecRep nes@(NewEpochState {..}) = do
    netId <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkNewEpochState
        <$> toSpecRep nesEL
        <*> toSpecRep nesBprev
        <*> toSpecRep nesBcur
        <*> withCtxSpecTransM netId (toSpecRep nesEs)
        <*> toSpecRep nesRu
        <*> (filterZeroEntries <$> toSpecRep (nes ^. nesStakePoolDistrG))
        <*> toSpecRep go
    where
      go = ssLeiosCommittee . ssStakeSet . esSnapshots $ nesEs
      -- The specification does not include zero entries in general
      -- while the implementation might. So we filter them out here for the sake
      -- of comparing results.
      --
      -- The discrepancy is discussed here:
      -- https://github.com/IntersectMBO/cardano-ledger/issues/5306
      filterZeroEntries (Agda.MkHSMap lst) =
        Agda.MkHSMap $ filter ((/= 0) . snd) lst
