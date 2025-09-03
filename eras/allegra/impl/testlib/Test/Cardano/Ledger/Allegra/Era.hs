{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Era (
  module Test.Cardano.Ledger.Shelley.Era,
  AllegraEraTest,
) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Plutus (emptyCostModels)
import Paths_cardano_ledger_allegra
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Shelley.Era

class
  ( ShelleyEraTest era
  , AllegraEraTxBody era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  ) =>
  AllegraEraTest era

instance EraTest AllegraEra where
  zeroCostModels = emptyCostModels

  getEraDataFileName = getDataFileName

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

  accountsToUMap = shelleyAccountsToUMap

instance ShelleyEraTest AllegraEra

instance AllegraEraTest AllegraEra
