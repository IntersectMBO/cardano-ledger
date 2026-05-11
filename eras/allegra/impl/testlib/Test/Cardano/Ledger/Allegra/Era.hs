{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Era (
  module Test.Cardano.Ledger.Shelley.Era,
  AllegraEraTest,
) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.MemoBytes (EqRaw)
import Cardano.Ledger.Plutus (emptyCostModels)
import Paths_cardano_ledger_allegra (getDataFileName)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Allegra.Binary.Annotator ()
import Test.Cardano.Ledger.Allegra.Examples (exampleAllegraTx)
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Era
import Test.Cardano.Ledger.Shelley.Examples (
  exampleShelleyPParams,
  exampleShelleyPParamsUpdate,
 )

class
  ( ShelleyEraTest era
  , AllegraEraTxBody era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , Arbitrary (NativeScript era)
  , EqRaw (NativeScript era)
  , SafeToHash (NativeScript era)
  , ToExpr (NativeScript era)
  ) =>
  AllegraEraTest era

instance EraTest AllegraEra where
  zeroCostModels = emptyCostModels

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

  mkEraFullPath = getDataFileName

  exampleTx = exampleAllegraTx

  examplePParams = exampleShelleyPParams

  examplePParamsUpdate = exampleShelleyPParamsUpdate

instance ShelleyEraTest AllegraEra

instance AllegraEraTest AllegraEra
