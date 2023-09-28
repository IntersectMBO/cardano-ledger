{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.UTxO () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Tx ()
import Cardano.Ledger.Allegra.TxBody ()
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.UTxO (
  ShelleyScriptsNeeded (..),
  getConsumedCoin,
  getShelleyScriptsNeeded,
  shelleyProducedValue,
 )
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..))
import Lens.Micro

instance Crypto c => EraUTxO (AllegraEra c) where
  type ScriptsNeeded (AllegraEra c) = ShelleyScriptsNeeded (AllegraEra c)

  getConsumedValue pp lookupKeyDeposit _ = getConsumedCoin pp lookupKeyDeposit

  getProducedValue = shelleyProducedValue

  getScriptsProvided _ tx = ScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptHashes) = scriptHashes
