{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert (nameGovCert) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Governance (VotingProcedures)
import Cardano.Ledger.Conway.TxCert
import Constrained
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
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
    second (Agda.MkCertState dState pState) . unComputationResult $
      Agda.govCertStep env vState sig

nameGovCert :: ConwayGovCert -> String
nameGovCert (ConwayRegDRep {}) = "ConwayRegDRep"
nameGovCert (ConwayUnRegDRep {}) = "ConwayUnRegDRep"
nameGovCert (ConwayUpdateDRep {}) = "ConwayUpdateDRep"
nameGovCert (ConwayAuthCommitteeHotKey {}) = "ConwayAuthCommitteeHotKey"
nameGovCert (ConwayResignCommitteeColdKey {}) = "ConwayResignCommitteeColdKey"
