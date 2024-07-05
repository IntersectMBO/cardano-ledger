{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERT rule
module Test.Cardano.Ledger.Constrained.Conway.Cert where

import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Shelley.API.Types

import Constrained

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Conway.Deleg
import Test.Cardano.Ledger.Constrained.Conway.GovCert
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams
import Test.Cardano.Ledger.Constrained.Conway.Pool

certEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (CertEnv (ConwayEra StandardCrypto))
certEnvSpec =
  constrained $ \ce ->
    match ce $ \_ pp _ _ _ ->
      satisfies pp pparamsSpec

certStateSpec ::
  IsConwayUniv fn =>
  Specification fn (CertState (ConwayEra StandardCrypto))
certStateSpec =
  constrained $ \cs ->
    match cs $ \vState pState dState ->
      [ satisfies vState vStateSpec
      , satisfies pState pStateSpec
      , satisfies dState dStateSpec
      ]

txCertSpec ::
  IsConwayUniv fn =>
  CertEnv (ConwayEra StandardCrypto) ->
  CertState (ConwayEra StandardCrypto) ->
  Specification fn (ConwayTxCert (ConwayEra StandardCrypto))
txCertSpec (CertEnv slot pp ce cc cp) CertState {..} =
  constrained $ \txCert ->
    caseOn
      txCert
      (branch $ \delegCert -> satisfies delegCert $ delegCertSpec delegEnv certDState)
      (branch $ \poolCert -> satisfies poolCert $ poolCertSpec poolEnv certPState)
      (branch $ \govCert -> satisfies govCert $ govCertSpec govCertEnv certVState)
  where
    delegEnv = ConwayDelegEnv pp (psStakePoolParams certPState)
    poolEnv = PoolEnv slot pp
    govCertEnv = ConwayGovCertEnv pp ce cc cp
