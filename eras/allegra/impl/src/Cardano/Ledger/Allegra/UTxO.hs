{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.UTxO () where

import Cardano.Ledger.Allegra.CertState ()
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Shelley.UTxO (
  ShelleyScriptsNeeded (..),
  getConsumedCoin,
  getShelleyMinFeeTxUtxo,
  getShelleyScriptsNeeded,
  getShelleyWitsVKeyNeeded,
  shelleyProducedValue,
 )
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided (..))
import Lens.Micro

instance EraUTxO AllegraEra where
  type ScriptsNeeded AllegraEra = ShelleyScriptsNeeded AllegraEra

  getConsumedValue pp lookupKeyDeposit _ = getConsumedCoin pp lookupKeyDeposit

  getProducedValue = shelleyProducedValue

  getScriptsProvided _ tx = ScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptHashes) = scriptHashes

  getWitsVKeyNeeded = getShelleyWitsVKeyNeeded

  getMinFeeTxUtxo pp tx _ = getShelleyMinFeeTxUtxo pp tx
