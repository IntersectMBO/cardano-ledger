{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
import Paths_cardano_ledger_babbage (getDataFileName)
import Test.Cardano.Ledger.Alonzo.Era
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Binary.Annotator ()
import Test.Cardano.Ledger.Babbage.Examples (
  exampleBabbageOnwardsEraPParams,
  exampleBabbagePParamsUpdate,
  exampleBabbageTx,
 )
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
  type
    EraRulesWithFailures BabbageEra =
      '[ "BBODY"
       , "DELEG"
       , "DELEGS"
       , "DELPL"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "PPUP"
       , "UTXO"
       , "UTXOS"
       , "UTXOW"
       ]
  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV2]

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

  mkEraFullPath = getDataFileName

  exampleTx = exampleBabbageTx

  examplePParams = exampleBabbageOnwardsEraPParams

  examplePParamsUpdate = exampleBabbagePParamsUpdate

instance ShelleyEraTest BabbageEra

instance AllegraEraTest BabbageEra

instance MaryEraTest BabbageEra

instance AlonzoEraTest BabbageEra

instance BabbageEraTest BabbageEra
