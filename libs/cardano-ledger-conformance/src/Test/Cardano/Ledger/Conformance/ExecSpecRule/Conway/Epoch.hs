{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Epoch () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (GovActionState)
import Control.State.Transition.Extended (TRC (..))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTRC (..),
  SpecTranslate (..),
  runSpecTransM,
  unComputationResult_,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()

instance ExecSpecRule "EPOCH" ConwayEra where
  type ExecContext "EPOCH" ConwayEra = [GovActionState ConwayEra]

  translateInputs _ (TRC (env, st, sig)) =
    runSpecTransM () $ SpecTRC <$> toSpecRep env <*> toSpecRep @ConwayEra st <*> toSpecRep sig

  runAgdaRuleWithDebug (SpecTRC env st sig) = unComputationResult_ $ Agda.epochStep env st sig
