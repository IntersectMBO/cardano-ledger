{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Constrained.V2.Conway.CertState where

import Cardano.Ledger.Api
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Shelley.API.Types

import Constrained

import Test.Cardano.Ledger.Constrained.V2.Conway
import Test.Cardano.Ledger.Constrained.V2.Conway.DelegCert
import Test.Cardano.Ledger.Constrained.V2.Conway.GovCert
import Test.Cardano.Ledger.Constrained.V2.Conway.PParams
import Test.Cardano.Ledger.Constrained.V2.Conway.PoolCert

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
