{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Ledgers () where

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (EnactState)
import Constrained (Specification (..))
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  unComputationResult,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Constrained.Conway (IsConwayUniv)

instance IsConwayUniv fn => ExecSpecRule fn "LEDGERS" Conway where
  type ExecContext fn "LEDGERS" Conway = EnactState Conway

  environmentSpec _ = TrueSpec
  stateSpec _ _ = TrueSpec
  signalSpec _ _ _ = TrueSpec
  runAgdaRule env st sig = unComputationResult $ Agda.ledgersStep env st sig
