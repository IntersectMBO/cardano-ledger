{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert (nameGovCert) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Constrained.Conway

instance IsConwayUniv fn => ExecSpecRule fn "GOVCERT" Conway where
  type ExecContext fn "GOVCERT" Conway = ConwayCertExecContext Conway

  environmentSpec _ctx = govCertEnvSpec

  stateSpec _ctx _env = vStateSpec

  signalSpec _ctx env st = govCertSpec env st

  classOf = Just . nameGovCert

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.govCertStep env st sig

nameGovCert :: ConwayGovCert c -> String
nameGovCert (ConwayRegDRep {}) = "ConwayRegDRep"
nameGovCert (ConwayUnRegDRep {}) = "ConwayUnRegDRep"
nameGovCert (ConwayUpdateDRep {}) = "ConwayUpdateDRep"
nameGovCert (ConwayAuthCommitteeHotKey {}) = "ConwayAuthCommitteeHotKey"
nameGovCert (ConwayResignCommitteeColdKey {}) = "ConwayResignCommitteeColdKey"
