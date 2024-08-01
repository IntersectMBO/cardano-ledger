{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERT rule
module Test.Cardano.Ledger.Constrained.Conway.Cert where

import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Shelley.API.Types

import Constrained

import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Conway.Deleg
import Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit (
  DeltaExecEnv (..),
  agdaDepositFromCertstate,
 )
import Test.Cardano.Ledger.Constrained.Conway.GovCert
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams
import Test.Cardano.Ledger.Constrained.Conway.Pool
import Test.QuickCheck (Gen)

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
      -- These weights try to make it equally likely that each of the many certs
      -- across the 3 categories are chosen at similar frequencies.
      (branchW 3 $ \delegCert -> satisfies delegCert $ conwayDelegCertSpec delegEnv certDState)
      (branchW 1 $ \poolCert -> satisfies poolCert $ poolCertSpec poolEnv certPState)
      (branchW 3 $ \govCert -> satisfies govCert $ govCertSpec govCertEnv certVState)
  where
    delegEnv = ConwayDelegEnv pp (psStakePoolParams certPState)
    poolEnv = PoolEnv slot pp
    govCertEnv = ConwayGovCertEnv pp ce cc cp

-- =================================================================

certExecEnvSpec ::
  IsConwayUniv fn =>
  (CertEnv Conway, CertState Conway) ->
  Specification fn (DeltaExecEnv (CertEnv Conway) Conway)
certExecEnvSpec (certEnv, certState) = constrained $ \ [var| deEnv |] ->
  match deEnv $ \ [var|env|] [var|deposits|] [var|withdrawal|] [var|votes|] ->
    [ assert $ env ==. lit certEnv
    , match votes $ \m -> sizeOf_ m ==. 0
    , assert $ deposits ==. lit (agdaDepositFromCertstate certState)
    , genHint 3 withdrawal -- Not sure if this is needed for CERT
    --    , assert $ forAll withdrawal (\p -> elem_ p (lit (possibleWithdrawal dstate)))
    ]

genCERTEnv :: Gen (CertEnv Conway, CertState Conway)
genCERTEnv = do
  env <- genFromSpec @ConwayFn certEnvSpec
  state <- genFromSpec @ConwayFn certStateSpec
  pure (env, state)
