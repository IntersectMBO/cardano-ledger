{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.UTxO (
  getBabbageSpendingDatum,
) where

import Cardano.Ledger.Alonzo.Scripts.Data (Data)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, ScriptPurpose)
import Cardano.Ledger.Alonzo.TxOut (dataHashTxOutL)
import Cardano.Ledger.Alonzo.TxWits (unTxDats)
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded,
  getAlonzoScriptsHashesNeeded,
  getAlonzoScriptsNeeded,
  getAlonzoSpendingTxIn,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Tx ()
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (dataTxOutL))
import Cardano.Ledger.Babbage.TxWits (datsTxWitsL)
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Shelley.UTxO (shelleyProducedValue)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Control.Applicative
import qualified Data.Map.Strict as Map
import Lens.Micro

instance Crypto c => EraUTxO (BabbageEra c) where
  type ScriptsNeeded (BabbageEra c) = AlonzoScriptsNeeded (BabbageEra c)

  getConsumedValue pp lookupKeyDeposit _ = getConsumedMaryValue pp lookupKeyDeposit

  getProducedValue = shelleyProducedValue

  getScriptsNeeded = getAlonzoScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

instance Crypto c => AlonzoEraUTxO (BabbageEra c) where
  getSpendingDatum = getBabbageSpendingDatum

-- | Extract binary data either directly from the `Tx` as an "inline datum"
-- or look it up in the witnesses by the hash.
getBabbageSpendingDatum ::
  ( AlonzoEraTx era
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  Tx era ->
  ScriptPurpose era ->
  Maybe (Data era)
getBabbageSpendingDatum (UTxO utxo) tx sp = do
  txIn <- getAlonzoSpendingTxIn sp
  txOut <- Map.lookup txIn utxo
  let txOutDataFromWits = do
        dataHash <- strictMaybeToMaybe (txOut ^. dataHashTxOutL)
        Map.lookup dataHash (unTxDats (tx ^. witsTxL . datsTxWitsL))
  strictMaybeToMaybe (txOut ^. dataTxOutL) <|> txOutDataFromWits
