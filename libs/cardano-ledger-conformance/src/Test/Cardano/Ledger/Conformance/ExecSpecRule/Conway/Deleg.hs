{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (nameDelegCert) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Rules (ConwayDelegEnv (..))
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..))
import Cardano.Ledger.Shelley.LedgerState (DState)
import Constrained
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit (DeltaExecEnv (..))

instance IsConwayUniv fn => ExecSpecRule fn "DELEG" Conway where
  type
    ExecContext fn "DELEG" Conway =
      (ConwayDelegEnv Conway, DState Conway)

  genExecContext = genDELEGDeltaEnv

  type ExecEnvironment fn "DELEG" Conway = DeltaExecEnv (ConwayDelegEnv Conway) Conway

  environmentSpec context = delegExecEnvSpec context

  stateSpec (_env, state) _deltaExecEnv = constrained $ \x -> x ==. lit state

  signalSpec (denv, _state) _deltaExecEnv state =
    constrained $ \sig -> satisfies sig (conwayDelegCertSpec denv state)

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.delegStep env st sig

  classOf = Just . nameDelegCert

nameDelegCert :: ConwayDelegCert c -> String
nameDelegCert ConwayRegCert {} = "RegKey"
nameDelegCert ConwayUnRegCert {} = "UnRegKey"
nameDelegCert ConwayDelegCert {} = "DelegateWithKey"
nameDelegCert ConwayRegDelegCert {} = "RegKey&Delegate"
