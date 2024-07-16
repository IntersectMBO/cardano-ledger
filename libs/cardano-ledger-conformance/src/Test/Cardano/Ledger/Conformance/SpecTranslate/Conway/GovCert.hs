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

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.GovCert () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState (
  CommitteeAuthorization (..),
  csCommitteeCreds,
  drepExpiry,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayGovCertEnv (..),
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Conway.TxCert (
  ConwayGovCert (..),
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core
import Test.Cardano.Ledger.Conway.TreeDiff (showExpr)

instance SpecTranslate ctx (ConwayGovCert c) where
  type SpecRep (ConwayGovCert c) = Agda.TxCert

  toSpecRep (ConwayRegDRep c d _) =
    Agda.RegDRep
      <$> toSpecRep c
      <*> toSpecRep d
      <*> pure ()
  toSpecRep (ConwayUnRegDRep c _) =
    Agda.DeRegDRep
      <$> toSpecRep c
  toSpecRep (ConwayUpdateDRep c _) =
    Agda.RegDRep
      <$> toSpecRep c
      <*> pure 0
      <*> pure ()
  toSpecRep (ConwayAuthCommitteeHotKey c h) =
    Agda.CCRegHot
      <$> toSpecRep c
      <*> toSpecRep (SJust h)
  toSpecRep (ConwayResignCommitteeColdKey c _) =
    Agda.CCRegHot
      <$> toSpecRep c
      <*> toSpecRep (SNothing @(Credential _ _))

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  ) =>
  SpecTranslate ctx (ConwayGovCertEnv era)
  where
  type SpecRep (ConwayGovCertEnv era) = Agda.CertEnv

  toSpecRep ConwayGovCertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
    Agda.MkCertEnv
      <$> toSpecRep cgceCurrentEpoch
      <*> toSpecRep cgcePParams
      <*> toSpecRep votes
      <*> toSpecRep withdrawals

instance SpecTranslate ctx (ConwayGovCertPredFailure era) where
  type SpecRep (ConwayGovCertPredFailure era) = OpaqueErrorString

  toSpecRep = pure . OpaqueErrorString . showExpr

instance SpecTranslate ctx (VState era) where
  type SpecRep (VState era) = Agda.GState

  toSpecRep VState {..} =
    Agda.MkGState
      <$> toSpecRep (drepExpiry <$> vsDReps)
      <*> toSpecRep
        (committeeCredentialToStrictMaybe <$> csCommitteeCreds vsCommitteeState)

committeeCredentialToStrictMaybe ::
  CommitteeAuthorization c ->
  StrictMaybe (Credential 'HotCommitteeRole c)
committeeCredentialToStrictMaybe (CommitteeHotCredential c) = SJust c
committeeCredentialToStrictMaybe (CommitteeMemberResigned _) = SNothing
