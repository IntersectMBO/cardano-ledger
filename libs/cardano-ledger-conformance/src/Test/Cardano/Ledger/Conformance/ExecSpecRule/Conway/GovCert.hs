{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert (nameGovCert) where

import Cardano.Ledger.CertState (VState (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Rules (ConwayGovCertEnv (..))
import Cardano.Ledger.Conway.TxCert
import Constrained
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit (DeltaExecEnv)

instance IsConwayUniv fn => ExecSpecRule fn "GOVCERT" Conway where
  type
    ExecContext fn "GOVCERT" Conway =
      (ConwayGovCertEnv Conway, VState Conway)

  genExecContext = genGOVCERTEnv

  type ExecEnvironment fn "GOVCERT" Conway = DeltaExecEnv (ConwayGovCertEnv Conway) Conway

  environmentSpec (env, state) = govcertExecEnvSpec (env, state)

  stateSpec (_env, state) _deltaExecEnv = constrained $ \x -> x ==. lit state

  signalSpec (env, _state) _deltaExecEnv state =
    constrained $ \sig -> satisfies sig (govCertSpec env state)

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
