{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (nameTxCert) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayTxCert (..))
import Data.Map.Strict (Map)
import Constrained
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
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
  ExecSpecRule fn "CERT" Conway
  where
  type ExecContext fn "CERT" Conway = (WitUniv Conway, ConwayCertExecContext Conway)

  genExecContext = do
    univ <- genWitUniv @Conway 200
    ccec <- genFromSpec @ConwayFn (conwayCertExecContextSpec univ)
    pure (univ, ccec)

  environmentSpec (univ, _) = certEnvSpec @fn @Conway univ

  stateSpec (univ, ccecCtx) _ = certStateSpec @fn @Conway univ (ccecDelegatees ccecCtx)

  signalSpec (univ, _) env state = conwayTxCertSpec @fn @Conway univ env state

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.certStep env st sig

  classOf = Just . nameTxCert

nameTxCert :: ConwayTxCert ConwayEra -> String
nameTxCert (ConwayTxCertDeleg x) = nameDelegCert x
nameTxCert (ConwayTxCertPool x) = namePoolCert x
nameTxCert (ConwayTxCertGov x) = nameGovCert x
