{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Era (
  module Test.Cardano.Ledger.Alonzo.Era,
  BabbageEraTest,
) where

import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Plutus (Language (..))
import Paths_cardano_ledger_babbage
import Test.Cardano.Ledger.Alonzo.Era
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

class
  ( AlonzoEraTest era
  , BabbageEraTxBody era
  , BabbageEraPParams era
  ) =>
  BabbageEraTest era

instance EraTest BabbageEra where
  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV2]

  getEraDataFileName = getDataFileName

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

  accountsToUMap = shelleyAccountsToUMap

instance ShelleyEraTest BabbageEra

instance AllegraEraTest BabbageEra

instance MaryEraTest BabbageEra

instance AlonzoEraTest BabbageEra

instance BabbageEraTest BabbageEra
