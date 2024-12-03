{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (nameDelegCert) where

import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Constrained
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (
  ConwayCertExecContext (..),
  conwayCertExecContextSpec,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

instance
  Inject
    (WitUniv Conway, ConwayCertExecContext Conway)
    (Set (Credential 'DRepRole StandardCrypto))
  where
  inject (_, x) = ccecDelegatees x

instance IsConwayUniv fn => ExecSpecRule fn "DELEG" Conway where
  type ExecContext fn "DELEG" Conway = (WitUniv Conway, ConwayCertExecContext Conway)

  genExecContext = do
    univ <- genWitUniv @Conway 300
    ccec <- genFromSpec @ConwayFn (conwayCertExecContextSpec univ 5)
    pure (univ, ccec)

  environmentSpec _ = delegEnvSpec

  stateSpec (univ, ccec) _env =
    certStateSpec @_ @Conway univ (ccecDelegatees ccec) (ccecWithdrawals ccec)

  signalSpec _ = conwayDelegCertSpec

  runAgdaRule env (Agda.MkCertState dState pState vState) sig =
    second
      (\dState' -> Agda.MkCertState dState' pState vState)
      . unComputationResult
      $ Agda.delegStep env dState sig

  classOf = Just . nameDelegCert

nameDelegCert :: ConwayDelegCert -> String
nameDelegCert ConwayRegCert {} = "RegKey"
nameDelegCert ConwayUnRegCert {} = "UnRegKey"
nameDelegCert ConwayDelegCert {} = "DelegateWithKey"
nameDelegCert ConwayRegDelegCert {} = "RegK&DelegateWithKey"
