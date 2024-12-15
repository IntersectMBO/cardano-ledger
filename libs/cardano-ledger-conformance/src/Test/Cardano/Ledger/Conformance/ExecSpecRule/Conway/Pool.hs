{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Pool where

import Cardano.Ledger.Conway
import Cardano.Ledger.Core (PoolCert (..))
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Constrained.Conway

instance IsConwayUniv fn => ExecSpecRule fn "POOL" ConwayEra where
  environmentSpec _ = poolEnvSpec

  stateSpec _ _ = pStateSpec

  signalSpec _ = poolCertSpec

  runAgdaRule env st sig = unComputationResult $ Agda.poolStep env st sig

  classOf = Just . namePoolCert

namePoolCert :: PoolCert -> String
namePoolCert RegPool {} = "RegPool"
namePoolCert RetirePool {} = "RetirePool"
