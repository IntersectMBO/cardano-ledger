{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.GovCert () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Data.Default (Default (..))
import Data.Map.Strict (Map, keysSet)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base

instance SpecTranslate DijkstraEra ConwayGovCert where
  type SpecRep DijkstraEra ConwayGovCert = Agda.DCert

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

instance SpecTranslate DijkstraEra (Conway.ConwayGovCertEnv DijkstraEra) where
  type SpecRep DijkstraEra (Conway.ConwayGovCertEnv DijkstraEra) = Agda.CertEnv
  type
    SpecContext DijkstraEra (Conway.ConwayGovCertEnv DijkstraEra) =
      (VotingProcedures DijkstraEra, Map AccountAddress Coin)

  -- TODO Dijkstra: the new ceDirectDeposits field defaults to empty here; the
  -- value should be plumbed from the transaction body's directDeposits.
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
        <*> pure (Agda.MkHSMap [])

instance SpecTranslate DijkstraEra (VState DijkstraEra) where
  type SpecRep DijkstraEra (VState DijkstraEra) = Agda.GState

  toSpecRep VState {..} = do
    Agda.MkGState
      <$> toSpecRepMap (updateExpiry . drepExpiry <$> vsDReps)
      <*> toSpecRepMap
        (committeeCredentialToStrictMaybe <$> csCommitteeCreds vsCommitteeState)
      <*> deposits
    where
      transEntry (cred, val) =
        (,) <$> toSpecRep cred <*> toSpecRep (drepDeposit val)
      deposits = Agda.MkHSMap <$> traverse transEntry (Map.toList vsDReps)
      updateExpiry = binOpEpochNo (+) vsNumDormantEpochs
