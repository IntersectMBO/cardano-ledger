{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (nameDelegCert) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..))
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Constrained.Conway

instance IsConwayUniv fn => ExecSpecRule fn "DELEG" Conway where
  environmentSpec _ = delegEnvSpec

  stateSpec _ _ = certStateSpec

  signalSpec _ = delegCertSpec

  runAgdaRule env (Agda.MkCertState dState pState vState) sig =
    bimap
      (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      (\dState' -> Agda.MkCertState dState' pState vState)
      . computationResultToEither
      $ Agda.delegStep env dState sig

  classOf = Just . nameDelegCert

nameDelegCert :: ConwayDelegCert c -> String
nameDelegCert ConwayRegCert {} = "RegKey"
nameDelegCert ConwayUnRegCert {} = "UnRegKey"
nameDelegCert ConwayDelegCert {} = "DelegateWithKey"
nameDelegCert ConwayRegDelegCert {} = "RegK&DelegateWithKey"
