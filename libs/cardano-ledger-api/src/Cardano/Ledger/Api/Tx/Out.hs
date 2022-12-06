{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module is used for building and inspecting transaction outputs.
--
-- You'll find some examples below.
--
-- Let's start by defining the GHC extensions and imports.
--
-- >>> :set -XTypeApplications
-- >>> import Cardano.Ledger.Api.Era (Babbage)
-- >>> import Lens.Micro
-- >>> import Test.Cardano.Ledger.Babbage.Serialisation.Generators () -- Neded for doctests only
-- >>> import Test.QuickCheck -- Needed for doctests only
--
-- Here's an example on how to build a very basic Babbage era transaction output with a random
-- address and value, and without any datum or reference script.
--
-- >>> :{
-- quickCheck $ \addr val ->
--     let
--         -- Defining a Babbage era transaction output with some random address and value.
--         txOut = mkBasicTxOut @Babbage addr val
--      in
--         -- We verify that the transaction output contains our random address and value.
--         txOut ^. addrTxOutL == addr && txOut ^. valueTxOutL == val
-- :}
-- +++ OK, passed 100 tests.
module Cardano.Ledger.Api.Tx.Out
  ( module Cardano.Ledger.Api.Scripts.Data,
    EraTxOut (..),

    -- * Shelley, Allegra and Mary Era
    ShelleyTxOut,

    -- * Alonzo Era
    AlonzoTxOut,
    AlonzoEraTxOut (..),

    -- * Babbage Era
    BabbageTxOut,
    BabbageEraTxOut (..),
    setMinCoinTxOut,
    setMinCoinSizedTxOut,
  )
where

import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxOut (..), AlonzoTxOut)
import Cardano.Ledger.Api.Scripts.Data
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxOut (..), BabbageTxOut)
import Cardano.Ledger.Binary
import Cardano.Ledger.Core (EraTxOut (..), PParams, coinTxOutL, eraProtVerLow)
import Cardano.Ledger.Shelley.TxBody (ShelleyTxOut)
import Lens.Micro

-- | Same as `setMinCoinSizedTxOut`, except it doesn't require the size of the
-- TxOut and will recompute it if needed. Initial amount is not important.
setMinCoinTxOut :: EraTxOut era => PParams era -> TxOut era -> TxOut era
setMinCoinTxOut pp = go
  where
    go txOut =
      let curMinCoin = getMinCoinTxOut pp txOut
          curCoin = txOut ^. coinTxOutL
       in if curCoin == curMinCoin
            then txOut
            else go (txOut & coinTxOutL .~ curMinCoin)

-- | This function will adjust the output's `Coin` value to the smallest amount
-- allowed by the UTXO rule. Initial amount is not important.
setMinCoinSizedTxOut ::
  forall era.
  EraTxOut era =>
  PParams era ->
  Sized (TxOut era) ->
  Sized (TxOut era)
setMinCoinSizedTxOut pp = go
  where
    version = eraProtVerLow @era
    go txOut =
      let curMinCoin = getMinCoinSizedTxOut pp txOut
          curCoin = txOut ^. toSizedL version coinTxOutL
       in if curCoin == curMinCoin
            then txOut
            else go (txOut & toSizedL version coinTxOutL .~ curMinCoin)
