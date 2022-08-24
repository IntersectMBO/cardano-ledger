{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Api.Tx.Out
  ( EraTxOut (..),

    -- * Shelley Era
    ShelleyTxOut,

    -- * Alonzo Era
    AlonzoTxOut,
    AlonzoEraTxOut (..),

    -- * Babbage
    BabbageTxOut,
    BabbageEraTxOut (..),
    setBabbageMinTxOut,
  )
where

import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxOut (..), AlonzoTxOut)
import Cardano.Ledger.Babbage.Rules (babbageMinUTxOValue)
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxOut (..), BabbageTxOut)
import Cardano.Ledger.Coin
import Cardano.Ledger.Core (EraTxOut (..), PParams, coinTxOutL)
import Cardano.Ledger.Serialization
import Cardano.Ledger.Shelley.TxBody (ShelleyTxOut)
import GHC.Records
import Lens.Micro

-- | This function will adjust the output's `Coin` value to the smallest amount
-- allowed by the UTXO rule starting with BabbageEra. Initial amount is not
-- important.
setBabbageMinTxOut ::
  (EraTxOut era, HasField "_coinsPerUTxOByte" (PParams era) Coin) =>
  PParams era ->
  Sized (TxOut era) ->
  Sized (TxOut era)
setBabbageMinTxOut pp = go
  where
    go txOut =
      let curMinCoin = babbageMinUTxOValue pp txOut
          curCoin = txOut ^. toSizedL coinTxOutL
       in if curCoin == curMinCoin
            then txOut
            else go (txOut & toSizedL coinTxOutL .~ curMinCoin)
