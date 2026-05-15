{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Epoch () where

import Cardano.Ledger.Conway (ConwayEra)
import Control.State.Transition.Extended (TRC (..))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  SpecTRC (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  unComputationResult_,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()

instance ExecSpecRule "EPOCH" ConwayEra where
  translateInputs (TRC (env, st, sig)) =
    SpecTRC <$> toSpecRep env <*> toSpecRep st <*> toSpecRep sig

  runAgdaRuleWithDebug (SpecTRC env st sig) = unComputationResult_ $ Agda.epochStep env st sig
