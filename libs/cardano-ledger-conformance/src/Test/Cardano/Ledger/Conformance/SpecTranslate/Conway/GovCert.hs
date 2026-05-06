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
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Credential (Credential (..))
import Data.Default (Default (..))
import Data.Map.Strict (Map, keysSet)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base

instance SpecTranslate ConwayEra ConwayGovCert where
  type SpecRep ConwayEra ConwayGovCert = Agda.DCert

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

instance SpecTranslate ConwayEra (Conway.ConwayGovCertEnv ConwayEra) where
  type SpecRep ConwayEra (Conway.ConwayGovCertEnv ConwayEra) = Agda.CertEnv
  type
    SpecContext ConwayEra (Conway.ConwayGovCertEnv ConwayEra) =
      (VotingProcedures ConwayEra, Map AccountAddress Coin)

  toSpecRep Conway.ConwayGovCertEnv {..} = do
    (votes, withdrawals) <- askSpecTransM
    let propGetCCMembers (UpdateCommittee _ _ x _) = Just $ keysSet x
        propGetCCMembers _ = Nothing
        potentialCCMembers =
          mconcat
            . mapMaybe (propGetCCMembers . pProcGovAction . gasProposalProcedure)
            . Map.elems
        ccColdCreds =
          foldMap (keysSet . committeeMembers) cgceCurrentCommittee
            <> potentialCCMembers cgceCommitteeProposals
    withCtxSpecTransM () $
      Agda.MkCertEnv
        <$> toSpecRep cgceCurrentEpoch
        <*> toSpecRep cgcePParams
        <*> toSpecRep votes
        <*> toSpecRepMap withdrawals
        <*> toSpecRep ccColdCreds

instance SpecTranslate ConwayEra (VState ConwayEra) where
  type SpecRep ConwayEra (VState ConwayEra) = Agda.GState

  toSpecRep VState {..} = do
    Agda.MkGState
      <$> (toSpecRepMap (updateExpiry . drepExpiry <$> vsDReps))
      <*> ( toSpecRepMap
              (committeeCredentialToStrictMaybe <$> csCommitteeCreds vsCommitteeState)
          )
      <*> deposits
    where
      transEntry (cred, val) =
        (,) <$> (Agda.DRepDeposit <$> toSpecRep cred) <*> toSpecRep (drepDeposit val)
      deposits = Agda.MkHSMap <$> traverse transEntry (Map.toList vsDReps)
      updateExpiry = binOpEpochNo (+) vsNumDormantEpochs
