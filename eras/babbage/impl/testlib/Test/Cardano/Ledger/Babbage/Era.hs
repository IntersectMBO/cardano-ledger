{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Era (
  module Test.Cardano.Ledger.Alonzo.Era,
  BabbageEraTest,
) where

import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Core
import Test.Cardano.Ledger.Alonzo.Era
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()

class
  ( AlonzoEraTest era
  , BabbageEraTxBody era
  , BabbageEraPParams era
  ) =>
  BabbageEraTest era

instance EraTest BabbageEra where
  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

  accountsToUMap = shelleyAccountsToUMap

instance ShelleyEraTest BabbageEra

instance AllegraEraTest BabbageEra

instance MaryEraTest BabbageEra

instance AlonzoEraTest BabbageEra

instance BabbageEraTest BabbageEra
