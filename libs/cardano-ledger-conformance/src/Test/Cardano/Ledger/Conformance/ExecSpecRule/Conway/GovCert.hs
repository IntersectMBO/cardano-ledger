{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Constrained.API
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

instance Inject (WitUniv ConwayEra, ConwayCertExecContext ConwayEra) (Map RewardAccount Coin) where
  inject (_, x) = ccecWithdrawals x

instance Inject (WitUniv ConwayEra, ConwayCertExecContext ConwayEra) (VotingProcedures ConwayEra) where
  inject (_, x) = ccecVotes x

instance ExecSpecRule "GOVCERT" ConwayEra where
  type ExecContext "GOVCERT" ConwayEra = (WitUniv ConwayEra, ConwayCertExecContext ConwayEra)

  genExecContext = do
    univ <- genWitUniv @ConwayEra 300
    ccec <- genFromSpec (conwayCertExecContextSpec univ 5)
    pure (univ, ccec)

  environmentSpec (univ, _) = govCertEnvSpec univ

  stateSpec (univ, ccec) _env =
    certStateSpec @ConwayEra univ (ccecDelegatees ccec) (ccecWithdrawals ccec)

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
