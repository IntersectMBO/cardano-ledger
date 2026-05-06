{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Ledgers () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (EnactState)
import Control.State.Transition.Extended (TRC (..))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  SpecTRC (..),
  SpecTranslate (..),
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (
  externalFunctions,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  runFromAgdaFunction,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()

instance ExecSpecRule "LEDGERS" ConwayEra where
  type ExecContext "LEDGERS" ConwayEra = EnactState ConwayEra

  translateInputs enactState (TRC (env, st, sig)) = do
    agdaEnv <- runSpecTransM enactState $ toSpecRep @ConwayEra env
    agdaSt <- runSpecTransM () $ toSpecRep @ConwayEra st
    agdaSig <- runSpecTransM () $ toSpecRep @ConwayEra sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  runAgdaRule = runFromAgdaFunction (Agda.ledgersStep externalFunctions)
