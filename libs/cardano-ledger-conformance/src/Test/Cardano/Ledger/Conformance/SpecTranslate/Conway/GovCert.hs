{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.GovCert () where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState (
  DRepState (..),
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
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.LedgerState
import Data.Default (Default (..))
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core
import Test.Cardano.Ledger.Conway.TreeDiff (showExpr)

instance Crypto c => SpecTranslate ctx (ConwayGovCert c) where
  type SpecRep (ConwayGovCert c) = Agda.DCert

  toSpecRep (ConwayRegDRep c d a) =
    Agda.Regdrep
      <$> toSpecRep c
      <*> toSpecRep d
      <*> toSpecRep (fromSMaybe def a)
  toSpecRep (ConwayUnRegDRep c d) =
    Agda.Deregdrep
      <$> toSpecRep c
      <*> toSpecRep d
  toSpecRep (ConwayUpdateDRep c a) =
    Agda.Regdrep
      <$> toSpecRep c
      <*> pure 0
      <*> toSpecRep (fromSMaybe def a)
  toSpecRep (ConwayAuthCommitteeHotKey c h) =
    Agda.Ccreghot
      <$> toSpecRep c
      <*> toSpecRep (SJust h)
  toSpecRep (ConwayResignCommitteeColdKey c _) =
    Agda.Ccreghot
      <$> toSpecRep c
      <*> toSpecRep (SNothing @(Credential _ _))

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map (RewardAccount (EraCrypto era)) Coin)
  ) =>
  SpecTranslate ctx (ConwayGovCertEnv era)
  where
  type SpecRep (ConwayGovCertEnv era) = Agda.CertEnv

  toSpecRep ConwayGovCertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map (RewardAccount (EraCrypto era)) Coin)
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

  toSpecRep VState {..} = do
    Agda.MkGState
      <$> toSpecRep (updateExpiry . drepExpiry <$> vsDReps)
      <*> toSpecRep
        (committeeCredentialToStrictMaybe <$> csCommitteeCreds vsCommitteeState)
      <*> toSpecRep deposits
    where
      deposits = Map.mapKeys DRepDeposit (drepDeposit <$> vsDReps)
      updateExpiry = binOpEpochNo (+) vsNumDormantEpochs
