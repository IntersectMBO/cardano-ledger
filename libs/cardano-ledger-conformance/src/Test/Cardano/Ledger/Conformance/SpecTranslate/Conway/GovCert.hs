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

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
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

instance SpecTranslate ctx ConwayEra ConwayGovCert where
  type SpecRep ConwayEra ConwayGovCert = Agda.DCert

  toSpecRep (ConwayRegDRep c d a) =
    Agda.Regdrep
      <$> toSpecRep @_ @ConwayEra c
      <*> toSpecRep @_ @ConwayEra d
      <*> toSpecRep @_ @ConwayEra (fromSMaybe def a)
  toSpecRep (ConwayUnRegDRep c d) =
    Agda.Deregdrep
      <$> toSpecRep @_ @ConwayEra c
      <*> toSpecRep @_ @ConwayEra d
  toSpecRep (ConwayUpdateDRep c a) =
    Agda.Regdrep
      <$> toSpecRep @_ @ConwayEra c
      <*> pure 0
      <*> toSpecRep @_ @ConwayEra (fromSMaybe def a)
  toSpecRep (ConwayAuthCommitteeHotKey c h) =
    Agda.Ccreghot
      <$> toSpecRep @_ @ConwayEra c
      <*> toSpecRep @_ @ConwayEra (SJust h)
  toSpecRep (ConwayResignCommitteeColdKey c _) =
    Agda.Ccreghot
      <$> toSpecRep @_ @ConwayEra c
      <*> toSpecRep @_ @ConwayEra (SNothing @(Credential _))

instance
  ( SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map AccountAddress Coin)
  ) =>
  SpecTranslate ctx ConwayEra (ConwayGovCertEnv era)
  where
  type SpecRep ConwayEra (ConwayGovCertEnv era) = Agda.CertEnv

  toSpecRep ConwayGovCertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map AccountAddress Coin)
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
      <$> toSpecRep @_ @ConwayEra cgceCurrentEpoch
      <*> toSpecRep @_ @ConwayEra cgcePParams
      <*> toSpecRep @_ @ConwayEra votes
      <*> toSpecRep @_ @ConwayEra withdrawals
      <*> toSpecRep @_ @ConwayEra ccColdCreds

instance SpecTranslate ctx ConwayEra (VState era) where
  type SpecRep ConwayEra (VState era) = Agda.GState

  toSpecRep VState {..} = do
    Agda.MkGState
      <$> toSpecRep @_ @ConwayEra (updateExpiry . drepExpiry <$> vsDReps)
      <*> toSpecRep @_ @ConwayEra
        (committeeCredentialToStrictMaybe <$> csCommitteeCreds vsCommitteeState)
      <*> deposits
    where
      transEntry (cred, val) =
        (,)
          <$> (Agda.DRepDeposit <$> toSpecRep @_ @ConwayEra cred)
          <*> toSpecRep @_ @ConwayEra (drepDeposit val)
      deposits = Agda.MkHSMap <$> traverse transEntry (Map.toList vsDReps)
      updateExpiry = binOpEpochNo (+) vsNumDormantEpochs
