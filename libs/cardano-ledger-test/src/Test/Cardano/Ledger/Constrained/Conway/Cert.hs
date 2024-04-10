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
  Spec fn (CertEnv (ConwayEra StandardCrypto))
certEnvSpec =
  constrained $ \ce ->
    match ce $ \_ pp _ ->
      satisfies pp pparamsSpec

certStateSpec ::
  IsConwayUniv fn =>
  Spec fn (CertState (ConwayEra StandardCrypto))
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
  Spec fn (ConwayTxCert (ConwayEra StandardCrypto))
txCertSpec (CertEnv slot pp ce) CertState {..} =
  constrained $ \txCert ->
    caseOn
      txCert
      (branch $ \delegCert -> satisfies delegCert $ delegCertSpec pp certDState)
      (branch $ \poolCert -> satisfies poolCert $ poolCertSpec (PoolEnv slot pp) certPState)
      (branch $ \govCert -> satisfies govCert $ govCertSpec (ConwayGovCertEnv pp ce) certVState)
