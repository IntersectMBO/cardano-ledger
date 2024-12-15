{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert (nameGovCert) where

import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert
import Constrained (lit)
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Constrained.Conway

instance
  ( IsConwayUniv fn
  , Inject (ConwayCertExecContext ConwayEra) (Map DepositPurpose Coin)
  ) =>
  ExecSpecRule fn "GOVCERT" ConwayEra
  where
  type ExecContext fn "GOVCERT" ConwayEra = ConwayCertExecContext ConwayEra

  environmentSpec _ctx = govCertEnvSpec

  stateSpec ctx _env = certStateSpec (lit $ ccecDelegatees ctx)

  signalSpec _ctx = govCertSpec

  classOf = Just . nameGovCert

  runAgdaRule env (Agda.MkCertState dState pState vState) sig =
    second (Agda.MkCertState dState pState) . unComputationResult $
      Agda.govCertStep env vState sig

nameGovCert :: ConwayGovCert -> String
nameGovCert (ConwayRegDRep {}) = "ConwayRegDRep"
nameGovCert (ConwayUnRegDRep {}) = "ConwayUnRegDRep"
nameGovCert (ConwayUpdateDRep {}) = "ConwayUpdateDRep"
nameGovCert (ConwayAuthCommitteeHotKey {}) = "ConwayAuthCommitteeHotKey"
nameGovCert (ConwayResignCommitteeColdKey {}) = "ConwayResignCommitteeColdKey"
