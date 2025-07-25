{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert () where

import Cardano.Ledger.Conway
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

instance ExecSpecRule "CERT" ConwayEra where
  type ExecContext "CERT" ConwayEra = (WitUniv ConwayEra, ConwayCertExecContext ConwayEra)
  
  translateInputs = undefined

  runAgdaRule = runFromAgdaFunction Agda.certStep
