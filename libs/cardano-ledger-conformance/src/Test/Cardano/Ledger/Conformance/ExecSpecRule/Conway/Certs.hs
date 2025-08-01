{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Certs () where

import Cardano.Ledger.Conway (ConwayEra)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), runFromAgdaFunction)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (ConwayCertExecContext)

instance ExecSpecRule "CERTS" ConwayEra where
  type ExecContext "CERTS" ConwayEra = ConwayCertExecContext ConwayEra

  runAgdaRule = runFromAgdaFunction Agda.certsStep
