{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert () where

import Cardano.Ledger.Conway (ConwayEra)
import Data.Bifunctor (Bifunctor (..))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), SpecTRC (..), unComputationResult)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (ConwayCertExecContext)

instance ExecSpecRule "GOVCERT" ConwayEra where
  type ExecContext "GOVCERT" ConwayEra = ConwayCertExecContext ConwayEra

  runAgdaRule (SpecTRC env (Agda.MkCertState dState pState vState) sig) =
    second (Agda.MkCertState dState pState) . unComputationResult $
      Agda.govCertStep env vState sig
