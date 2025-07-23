{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Epoch () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (GovActionState)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), SpecTRC (..), unComputationResult_)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()

instance ExecSpecRule "EPOCH" ConwayEra where
  type ExecContext "EPOCH" ConwayEra = [GovActionState ConwayEra]

  runAgdaRule (SpecTRC env st sig) = unComputationResult_ $ Agda.epochStep env st sig
