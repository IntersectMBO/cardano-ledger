{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

instance ExecSpecRule "POOL" ConwayEra where
  type ExecContext "POOL" ConwayEra = WitUniv ConwayEra
  environmentSpec ctx = poolEnvSpec ctx

  stateSpec ctx _ = pStateSpec ctx

  signalSpec wituniv env st = poolCertSpec @ConwayEra wituniv env st

  runAgdaRule env st sig = unComputationResult $ Agda.poolStep env st sig

  classOf = Just . namePoolCert

namePoolCert :: PoolCert -> String
namePoolCert RegPool {} = "RegPool"
namePoolCert RetirePool {} = "RetirePool"
