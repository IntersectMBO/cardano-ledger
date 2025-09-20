{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Era (
  module Test.Cardano.Ledger.Alonzo.Era,
  BabbageEraTest,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Plutus (Language (..))
import Test.Cardano.Ledger.Alonzo.Era
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

class
  ( AlonzoEraTest era
  , BabbageEraTxBody era
  , BabbageEraPParams era
  , EraPlutusTxInfo PlutusV2 era
  ) =>
  BabbageEraTest era

instance EraTest BabbageEra where
  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV2]
  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

instance ShelleyEraTest BabbageEra

instance AllegraEraTest BabbageEra

instance MaryEraTest BabbageEra

instance AlonzoEraTest BabbageEra

instance BabbageEraTest BabbageEra
