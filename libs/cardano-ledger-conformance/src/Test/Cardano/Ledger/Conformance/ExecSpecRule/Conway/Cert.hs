{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (nameTxCert) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayTxCert (..))
import Constrained
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (nameDelegCert)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert (nameGovCert)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Pool (namePoolCert)
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "CERT" ConwayEra
  where
  type ExecContext fn "CERT" ConwayEra = (WitUniv ConwayEra, ConwayCertExecContext ConwayEra)

  genExecContext = do
    univ <- genWitUniv @ConwayEra 300
    ccec <- genFromSpec @ConwayFn (conwayCertExecContextSpec univ 5)
    pure (univ, ccec)

  environmentSpec (univ, _) = certEnvSpec @fn @ConwayEra univ

  stateSpec (univ, ccec) _ =
    certStateSpec @ConwayEra @fn univ (ccecDelegatees ccec) (ccecWithdrawals ccec)

  signalSpec (univ, _) env state = conwayTxCertSpec @fn @ConwayEra univ env state

  runAgdaRule env st sig =
    unComputationResult $
      Agda.certStep env st sig

  classOf = Just . nameTxCert

nameTxCert :: ConwayTxCert ConwayEra -> String
nameTxCert (ConwayTxCertDeleg x) = nameDelegCert x
nameTxCert (ConwayTxCertPool x) = namePoolCert x
nameTxCert (ConwayTxCertGov x) = nameGovCert x
