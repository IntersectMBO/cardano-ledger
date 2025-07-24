{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Pool where

import Cardano.Ledger.Conway
import Cardano.Ledger.Core (PoolCert (..))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()

instance ExecSpecRule "POOL" ConwayEra where
  runAgdaRule env st sig = unComputationResult $ Agda.poolStep env st sig

  classOf = Just . namePoolCert

namePoolCert :: PoolCert -> String
namePoolCert RegPool {} = "RegPool"
namePoolCert RetirePool {} = "RetirePool"
