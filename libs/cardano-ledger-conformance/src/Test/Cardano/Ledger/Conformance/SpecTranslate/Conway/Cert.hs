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
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.LedgerState
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.VMap as VMap
import Lens.Micro
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Conway.TreeDiff

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  ) =>
  SpecTranslate ctx (CertEnv era)
  where
  type SpecRep (CertEnv era) = Agda.CertEnv
  toSpecRep CertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
    Agda.MkCertEnv
      <$> toSpecRep ceCurrentEpoch
      <*> toSpecRep cePParams
      <*> toSpecRep votes
      <*> toSpecRep withdrawals

instance SpecTranslate ctx (CertState era) where
  type SpecRep (CertState era) = Agda.CertState
  toSpecRep CertState {..} =
    Agda.MkCertState
      <$> toSpecRep certDState
      <*> toSpecRep certPState
      <*> toSpecRep certVState

instance SpecTranslate ctx (ConwayTxCert era) where
  type SpecRep (ConwayTxCert era) = Agda.TxCert

  toSpecRep (ConwayTxCertPool p) = toSpecRep p
  toSpecRep (ConwayTxCertGov c) = toSpecRep c
  toSpecRep (ConwayTxCertDeleg x) = toSpecRep x

instance
  ( ConwayEraGov era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecTranslate ctx (TxOut era)
  ) =>
  SpecTranslate ctx (LedgerState era)
  where
  type SpecRep (LedgerState era) = Agda.LedgerState

  toSpecRep (LedgerState {..}) =
    Agda.MkLedgerState
      <$> toSpecRep lsUTxOState
      <*> toSpecRep (utxosGovState lsUTxOState ^. proposalsGovStateL)
      <*> toSpecRep lsCertState

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
  , SpecTranslate ctx (TxOut era)
  ) =>
  SpecTranslate ctx (EpochState era)
  where
  type SpecRep (EpochState era) = Agda.EpochState

  toSpecRep (EpochState {esLState = esLState@LedgerState {lsUTxOState}, ..}) =
    Agda.MkEpochState
      <$> toSpecRep esAccountState
      <*> toSpecRep esSnapshots
      <*> toSpecRep esLState
      <*> toSpecRep enactState
      <*> toSpecRep ratifyState
    where
      enactState = mkEnactState $ utxosGovState lsUTxOState
      ratifyState = RatifyState enactState mempty mempty False

instance SpecTranslate ctx (SnapShots c) where
  type SpecRep (SnapShots c) = Agda.Snapshots

  toSpecRep (SnapShots {..}) =
    Agda.MkSnapshots
      <$> toSpecRep ssStakeMark
      <*> toSpecRep ssStakeSet
      <*> toSpecRep ssStakeGo
      <*> toSpecRep ssFee

instance SpecTranslate ctx (SnapShot c) where
  type SpecRep (SnapShot c) = Agda.Snapshot

  toSpecRep (SnapShot {..}) =
    Agda.MkSnapshot
      <$> toSpecRep ssStake
      <*> toSpecRep (VMap.toMap ssDelegations)

instance SpecTranslate ctx (Stake c) where
  type SpecRep (Stake c) = Agda.HSMap Agda.Credential Agda.Coin

  toSpecRep (Stake stake) = toSpecRep $ VMap.toMap stake

instance SpecTranslate ctx AccountState where
  type SpecRep AccountState = Agda.Acnt

  toSpecRep (AccountState {..}) =
    Agda.MkAcnt
      <$> toSpecRep asTreasury
      <*> toSpecRep asReserves

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
  , SpecTranslate ctx (TxOut era)
  ) =>
  SpecTranslate ctx (NewEpochState era)
  where
  type SpecRep (NewEpochState era) = Agda.NewEpochState

  toSpecRep (NewEpochState {..}) =
    Agda.MkNewEpochState
      <$> toSpecRep nesEL
      <*> toSpecRep nesEs

instance SpecTranslate ctx (ConwayNewEpochPredFailure era) where
  type SpecRep (ConwayNewEpochPredFailure era) = OpaqueErrorString
  toSpecRep = pure . OpaqueErrorString . show . toExpr
