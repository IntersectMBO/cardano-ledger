{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Pool where

import Cardano.Ledger.CertState (PState (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Core (PoolCert (..))
import Cardano.Ledger.Shelley.Rules (PoolEnv (..))
import Constrained
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit (DeltaExecEnv (..))

instance IsConwayUniv fn => ExecSpecRule fn "POOL" Conway where
  type ExecContext fn "POOL" Conway = (PoolEnv Conway, PState Conway)

  genExecContext = genPOOLEnv

  type ExecEnvironment fn "POOL" Conway = DeltaExecEnv (PoolEnv Conway) Conway

  environmentSpec (env, state) = poolExecEnvSpec (env, state)

  stateSpec (_env, state) _deltaExecEnv = constrained $ \x -> x ==. lit state

  signalSpec (env, _state) _deltaExecEnv state =
    constrained $ \sig -> satisfies sig (poolCertSpec env state)

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.poolStep env st sig

  classOf = Just . namePoolCert

namePoolCert :: PoolCert c -> String
namePoolCert RegPool {} = "RegPool"
namePoolCert RetirePool {} = "RetirePool"
