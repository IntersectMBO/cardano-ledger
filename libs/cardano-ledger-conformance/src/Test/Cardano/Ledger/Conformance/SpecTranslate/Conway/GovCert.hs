{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayGovCertEnv (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Credential (Credential (..))
import Data.Default (Default (..))
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map, keysSet)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base

instance SpecTranslate ctx ConwayGovCert where
  type SpecRep ConwayGovCert = Agda.DCert

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
      <*> toSpecRep (SNothing @(Credential _))

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map RewardAccount Coin)
  ) =>
  SpecTranslate ctx (ConwayGovCertEnv era)
  where
  type SpecRep (ConwayGovCertEnv era) = Agda.CertEnv

  toSpecRep ConwayGovCertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map RewardAccount Coin)
    let propGetCCMembers (UpdateCommittee _ _ x _) = Just $ keysSet x
        propGetCCMembers _ = Nothing
        potentialCCMembers =
          mconcat
            . mapMaybe (propGetCCMembers . pProcGovAction . gasProposalProcedure)
            . Map.elems
        ccColdCreds =
          foldMap (keysSet . committeeMembers) cgceCurrentCommittee
            <> potentialCCMembers cgceCommitteeProposals
    Agda.MkCertEnv
      <$> toSpecRep cgceCurrentEpoch
      <*> toSpecRep cgcePParams
      <*> toSpecRep votes
      <*> toSpecRep withdrawals
      <*> toSpecRep ccColdCreds

instance SpecTranslate ctx (VState era) where
  type SpecRep (VState era) = Agda.GState

  toSpecRep VState {..} = do
    Agda.MkGState
      <$> toSpecRep (updateExpiry . drepExpiry <$> vsDReps)
      <*> toSpecRep
        (committeeCredentialToStrictMaybe <$> csCommitteeCreds vsCommitteeState)
      <*> deposits
    where
      transEntry (cred, val) =
        (,) <$> (Agda.DRepDeposit <$> toSpecRep cred) <*> toSpecRep (drepDeposit val)
      deposits = Agda.MkHSMap <$> traverse transEntry (Map.toList vsDReps)
      updateExpiry = binOpEpochNo (+) vsNumDormantEpochs
