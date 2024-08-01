{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (nameTxCert) where

import Cardano.Ledger.CertState (CertState (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Rules (CertEnv)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert (..))
import Constrained (constrained, lit, satisfies, (==.))
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (nameDelegCert)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert (nameGovCert)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Pool (namePoolCert)
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit (DeltaExecEnv (..))

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "CERT" Conway
  where
  type ExecContext fn "CERT" Conway = (CertEnv Conway, CertState Conway)
  genExecContext = genCERTEnv

  type ExecEnvironment fn "CERT" Conway = DeltaExecEnv (CertEnv Conway) Conway
  environmentSpec context = certExecEnvSpec context

  stateSpec (_env, state) _deltaExecEnv = constrained $ \x -> x ==. lit state
  signalSpec (denv, _state) _deltaExecEnv state =
    constrained $ \sig -> satisfies sig (txCertSpec denv state)

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.certStep env st sig

  classOf = Just . nameTxCert

nameTxCert :: ConwayTxCert Conway -> String
nameTxCert (ConwayTxCertDeleg x) = nameDelegCert x
nameTxCert (ConwayTxCertPool x) = namePoolCert x
nameTxCert (ConwayTxCertGov x) = nameGovCert x
