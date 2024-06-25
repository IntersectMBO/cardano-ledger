{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the GOVCERT rule
module Test.Cardano.Ledger.Constrained.Conway.GovCert where

import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import qualified Data.Map as Map
import Lens.Micro

import Constrained

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams

vStateSpec :: Specification fn (VState (ConwayEra StandardCrypto))
vStateSpec = TrueSpec

govCertSpec ::
  IsConwayUniv fn =>
  ConwayGovCertEnv (ConwayEra StandardCrypto) ->
  VState (ConwayEra StandardCrypto) ->
  Specification fn (ConwayGovCert StandardCrypto)
govCertSpec ConwayGovCertEnv {..} vs =
  let reps = lit $ Map.keysSet $ vsDReps vs
      deposits = lit [(k, drepDeposit dep) | (k, dep) <- Map.toList $ vsDReps vs]
      getNewMembers = \case
        UpdateCommittee _ _ newMembers _ -> Map.keysSet newMembers
        _ -> mempty
      knownColdCreds =
        Map.keysSet (foldMap committeeMembers cgceCurrentCommittee)
          <> foldMap (getNewMembers . pProcGovAction . gasProposalProcedure) cgceCommitteeProposals
      ccCertSpec coldCred =
        assert . member_ coldCred $ lit knownColdCreds
   in constrained $ \gc ->
        caseOn
          gc
          -- ConwayRegDRep
          ( branch $ \key coin _ ->
              [ not_ $ member_ key reps
              , coin ==. lit (cgcePParams ^. ppDRepDepositL)
              ]
          )
          -- ConwayUnRegDRep
          ( branch $ \cred coin ->
              elem_ (pair_ cred coin) deposits
          )
          -- ConwayUpdateDRep
          ( branch $ \key _ ->
              member_ key reps
          )
          -- ConwayAuthCommitteeHotKey
          (branch $ \coldCred _ -> ccCertSpec coldCred)
          -- ConwayResignCommitteeColdKey
          (branch $ \coldCred _ -> ccCertSpec coldCred)

govCertEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (ConwayGovCertEnv (ConwayEra StandardCrypto))
govCertEnvSpec =
  constrained $ \gce ->
    match gce $ \pp _ _ _ ->
      satisfies pp pparamsSpec
