{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the GOVCERT rule
module Test.Cardano.Ledger.Constrained.V2.Conway.GOVCERT where

import Cardano.Ledger.Api
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import qualified Data.Map as Map
import Lens.Micro

import Constrained

import Test.Cardano.Ledger.Constrained.V2.Conway
import Test.Cardano.Ledger.Constrained.V2.Conway.PParams

vStateSpec :: Spec fn (VState (ConwayEra StandardCrypto))
vStateSpec = TrueSpec

govCertSpec ::
  IsConwayUniv fn =>
  ConwayGovCertEnv (ConwayEra StandardCrypto) ->
  VState (ConwayEra StandardCrypto) ->
  Spec fn (ConwayGovCert StandardCrypto)
govCertSpec ConwayGovCertEnv {..} vs =
  let reps = lit $ Map.keysSet $ vsDReps vs
      deposits = lit [(k, drepDeposit dep) | (k, dep) <- Map.toList $ vsDReps vs]
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
          (branch $ \_ _ -> True)
          -- ConwayResignCommitteeColdKey
          (branch $ \_ _ -> True)

govCertEnvSpec ::
  IsConwayUniv fn =>
  Spec fn (ConwayGovCertEnv (ConwayEra StandardCrypto))
govCertEnvSpec =
  constrained $ \gce ->
    match gce $ \pp _ ->
      satisfies pp pparamsSpec
