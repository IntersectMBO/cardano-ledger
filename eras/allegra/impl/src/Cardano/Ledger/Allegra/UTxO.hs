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
  getConsumedCoin,
  getShelleyMinFeeTxUtxo,
  getShelleyScriptsNeeded,
  getShelleyWitsVKeyNeeded,
  shelleyConsumed,
  shelleyProducedValue,
 )
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided (..))
import Lens.Micro

instance EraUTxO AllegraEra where
  type ScriptsNeeded AllegraEra = ShelleyScriptsNeeded AllegraEra

  consumed = shelleyConsumed

  getConsumedValue pp lookupKeyDeposit _ = getConsumedCoin pp lookupKeyDeposit

  getProducedValue pp isRegPoolId txBody =
    withTopTxLevelOnly txBody (shelleyProducedValue pp isRegPoolId)

  getScriptsProvided _ tx = ScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptHashes) = scriptHashes

  getWitsVKeyNeeded = getShelleyWitsVKeyNeeded

  getMinFeeTxUtxo pp tx _ = getShelleyMinFeeTxUtxo pp tx
