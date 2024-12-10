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

import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (StandardCrypto)
import Constrained (lit)
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Constrained.Conway

instance
  ( IsConwayUniv fn
  , Inject (ConwayCertExecContext Conway) (Map (DepositPurpose StandardCrypto) Coin)
  ) =>
  ExecSpecRule fn "GOVCERT" Conway
  where
  type ExecContext fn "GOVCERT" Conway = ConwayCertExecContext Conway

  environmentSpec _ctx = govCertEnvSpec

  stateSpec ctx _env = certStateSpec (lit $ ccecDelegatees ctx)

  signalSpec _ctx = govCertSpec

  classOf = Just . nameGovCert

  runAgdaRule env (Agda.MkCertState dState pState vState) sig =
    second (Agda.MkCertState dState pState) . unComputationResult $
      Agda.govCertStep env vState sig

nameGovCert :: ConwayGovCert c -> String
nameGovCert (ConwayRegDRep {}) = "ConwayRegDRep"
nameGovCert (ConwayUnRegDRep {}) = "ConwayUnRegDRep"
nameGovCert (ConwayUpdateDRep {}) = "ConwayUpdateDRep"
nameGovCert (ConwayAuthCommitteeHotKey {}) = "ConwayAuthCommitteeHotKey"
nameGovCert (ConwayResignCommitteeColdKey {}) = "ConwayResignCommitteeColdKey"
