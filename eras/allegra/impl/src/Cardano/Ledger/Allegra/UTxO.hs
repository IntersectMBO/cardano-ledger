{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.UTxO () where

import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.State ()
import Cardano.Ledger.Shelley.UTxO (
  ShelleyScriptsNeeded (..),
  ShelleyScriptsProvided (..),
  getConsumedCoin,
  getShelleyMinFeeTxUtxo,
  getShelleyScriptsNeeded,
  getShelleyWitsVKeyNeeded,
  shelleyConsumed,
  shelleyProducedValue,
 )
import Cardano.Ledger.State (EraUTxO (..))
import Lens.Micro

instance EraUTxO AllegraEra where
  type ScriptsNeeded AllegraEra = ShelleyScriptsNeeded AllegraEra
  type ScriptsProvided AllegraEra = ShelleyScriptsProvided AllegraEra

  consumed = shelleyConsumed

  getConsumedValue pp lookupKeyDeposit _ = getConsumedCoin pp lookupKeyDeposit

  getProducedValue pp isRegPoolId txBody =
    withTopTxLevelOnly txBody (shelleyProducedValue pp isRegPoolId)

  getScriptsProvided _ tx = ShelleyScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

  getScriptsProvidedMap = unShelleyScriptsProvided

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptHashes) = scriptHashes

  getWitsVKeyNeeded = getShelleyWitsVKeyNeeded

  getMinFeeTxUtxo pp tx _ = getShelleyMinFeeTxUtxo pp tx
