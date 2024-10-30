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

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Governance (VotingProcedures)
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (StandardCrypto)
import Constrained
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (
  ConwayCertExecContext (..),
  conwayCertExecContextSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

instance Inject (WitUniv Conway, ConwayCertExecContext Conway) (Map (RewardAccount StandardCrypto) Coin) where
  inject (_, x) = ccecWithdrawals x

instance Inject (WitUniv Conway, ConwayCertExecContext Conway) (VotingProcedures Conway) where
  inject (_, x) = ccecVotes x

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "GOVCERT" Conway
  where
  type ExecContext fn "GOVCERT" Conway = (WitUniv Conway, ConwayCertExecContext Conway)

  genExecContext = do
    univ <- genWitUniv @Conway 200
    ccec <- genFromSpec @ConwayFn (conwayCertExecContextSpec univ)
    pure (univ, ccec)

  environmentSpec (univ, _) = govCertEnvSpec univ

  -- stateSpec ctx _env = certStateSpec (lit $ ccecDelegatees ctx)
  stateSpec (univ, ccecCtx) _env = certStateSpec univ (ccecDelegatees ccecCtx)

  signalSpec (univ, _) = govCertSpec univ

  classOf = Just . nameGovCert

  runAgdaRule env (Agda.MkCertState dState pState vState) sig =
    bimap
      (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      (Agda.MkCertState dState pState)
      . computationResultToEither
      $ Agda.govCertStep env vState sig

nameGovCert :: ConwayGovCert c -> String
nameGovCert (ConwayRegDRep {}) = "ConwayRegDRep"
nameGovCert (ConwayUnRegDRep {}) = "ConwayUnRegDRep"
nameGovCert (ConwayUpdateDRep {}) = "ConwayUpdateDRep"
nameGovCert (ConwayAuthCommitteeHotKey {}) = "ConwayAuthCommitteeHotKey"
nameGovCert (ConwayResignCommitteeColdKey {}) = "ConwayResignCommitteeColdKey"
