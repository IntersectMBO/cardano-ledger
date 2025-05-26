{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Era (
  EraTest,
  ShelleyEraTest,
) where

import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.State
import Data.Default
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.TreeDiff ()

class
  ( EraTest era
  , ShelleyEraScript era
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , ToExpr (StashedAVVMAddresses era)
  , NFData (StashedAVVMAddresses era)
  , Default (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
  , ToExpr (ScriptsNeeded era)
  ) =>
  ShelleyEraTest era

instance EraTest ShelleyEra

instance ShelleyEraTest ShelleyEra
