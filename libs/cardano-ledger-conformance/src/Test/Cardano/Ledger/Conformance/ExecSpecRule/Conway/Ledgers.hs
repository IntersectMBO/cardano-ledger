{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Ledgers () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (EnactState)
import Constrained.API ()
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  unComputationResult,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()

instance ExecSpecRule "LEDGERS" ConwayEra where
  type ExecContext "LEDGERS" ConwayEra = EnactState ConwayEra

  environmentSpec _ = mempty
  stateSpec _ _ = mempty
  signalSpec _ _ _ = mempty
  runAgdaRule env st sig = unComputationResult $ Agda.ledgersStep env st sig
