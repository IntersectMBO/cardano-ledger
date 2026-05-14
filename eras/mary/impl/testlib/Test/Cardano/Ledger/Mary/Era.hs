{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Era (
  module Test.Cardano.Ledger.Allegra.Era,
  MaryEraTest,
) where

import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Plutus (emptyCostModels)
import Paths_cardano_ledger_mary (getDataFileName)
import Test.Cardano.Ledger.Allegra.Era
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.Binary.Annotator ()
import Test.Cardano.Ledger.Mary.Examples (exampleMaryTx)
import Test.Cardano.Ledger.Mary.TreeDiff ()
import Test.Cardano.Ledger.Shelley.Examples (
  exampleShelleyPParams,
  exampleShelleyPParamsUpdate,
 )

class
  ( AllegraEraTest era
  , MaryEraTxBody era
  ) =>
  MaryEraTest era

instance EraTest MaryEra where
  zeroCostModels = emptyCostModels

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

  mkEraFullPath = getDataFileName

  exampleTx = exampleMaryTx

  examplePParams = exampleShelleyPParams

  examplePParamsUpdate = exampleShelleyPParamsUpdate

instance ShelleyEraTest MaryEra

instance AllegraEraTest MaryEra

instance MaryEraTest MaryEra
