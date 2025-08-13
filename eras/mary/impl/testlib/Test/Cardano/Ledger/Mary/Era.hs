{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Era (
  module Test.Cardano.Ledger.Allegra.Era,
  MaryEraTest,
) where

import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Plutus (emptyCostModels)
import Paths_cardano_ledger_mary
import Test.Cardano.Ledger.Allegra.Era
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.TreeDiff ()

class
  ( AllegraEraTest era
  , MaryEraTxBody era
  ) =>
  MaryEraTest era

instance EraTest MaryEra where
  zeroCostModels = emptyCostModels

  getEraDataFileName = getDataFileName

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

  accountsToUMap = shelleyAccountsToUMap

instance ShelleyEraTest MaryEra

instance AllegraEraTest MaryEra

instance MaryEraTest MaryEra
