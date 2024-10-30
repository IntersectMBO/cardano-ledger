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
import Cardano.Ledger.Core
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse


instance IsConwayUniv fn => ExecSpecRule fn "POOL" Conway where
  type ExecContext fn "POOL" Conway = WitUniv Conway
  environmentSpec ctx = poolEnvSpec ctx

  stateSpec ctx _ = pStateSpec ctx

  signalSpec wituniv env st = poolCertSpec @fn @Conway wituniv env st

  runAgdaRule env st sig = unComputationResult $ Agda.poolStep env st sig

  classOf = Just . namePoolCert

namePoolCert :: PoolCert -> String
namePoolCert RegPool {} = "RegPool"
namePoolCert RetirePool {} = "RetirePool"
