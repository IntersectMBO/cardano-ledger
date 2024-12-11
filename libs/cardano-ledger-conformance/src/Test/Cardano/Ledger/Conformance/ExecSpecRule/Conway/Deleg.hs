{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (nameDelegCert) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Constrained (lit)
import Data.Bifunctor (Bifunctor (..))
import Data.Set (Set)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Constrained.Conway

instance IsConwayUniv fn => ExecSpecRule fn "DELEG" Conway where
  type ExecContext fn "DELEG" Conway = Set (Credential 'DRepRole StandardCrypto)

  environmentSpec _ = delegEnvSpec

  stateSpec ctx _ = certStateSpec (lit ctx)

  signalSpec _ = conwayDelegCertSpec

  runAgdaRule env (Agda.MkCertState dState pState vState) sig =
    second
      (\dState' -> Agda.MkCertState dState' pState vState)
      . unComputationResult
      $ Agda.delegStep env dState sig

  classOf = Just . nameDelegCert

nameDelegCert :: ConwayDelegCert c -> String
nameDelegCert ConwayRegCert {} = "RegKey"
nameDelegCert ConwayUnRegCert {} = "UnRegKey"
nameDelegCert ConwayDelegCert {} = "DelegateWithKey"
nameDelegCert ConwayRegDelegCert {} = "RegK&DelegateWithKey"
