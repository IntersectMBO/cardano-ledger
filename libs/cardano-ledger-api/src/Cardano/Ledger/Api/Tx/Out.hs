{-# LANGUAGE BangPatterns #-}
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
module Cardano.Ledger.Api.Tx.Out (
  module Cardano.Ledger.Api.Tx.Address,
  EraTxOut (TxOut),
  mkBasicTxOut,

  -- ** Value
  valueTxOutL,
  coinTxOutL,
  isAdaOnlyTxOutF,

  -- ** Address
  addrTxOutL,
  bootAddrTxOutF,

  -- ** Size
  getMinCoinTxOut,
  setMinCoinTxOut,
  getMinCoinSizedTxOut,
  setMinCoinSizedTxOut,
  ensureMinCoinTxOut,
  ensureMinCoinSizedTxOut,

  -- * Shelley, Allegra and Mary Era

  -- * Alonzo Era
  AlonzoEraTxOut,
  dataHashTxOutL,
  DataHash,
  datumTxOutF,

  -- * Babbage Era
  BabbageEraTxOut,
  dataTxOutL,
  Data (..),
  datumTxOutL,
  Datum (..),
  referenceScriptTxOutL,
)
where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..))
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Api.Scripts.Data (Data (..), DataHash, Datum (..))
import Cardano.Ledger.Api.Tx.Address
import Cardano.Ledger.Babbage.Core (BabbageEraTxOut (..))
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Core (
  EraTxOut (..),
  PParams,
  bootAddrTxOutF,
  coinTxOutL,
  eraProtVerLow,
  isAdaOnlyTxOutF,
 )
import Lens.Micro

setMinCoinTxOutInternal ::
  EraTxOut era =>
  (Coin -> Coin -> Bool) ->
  PParams era ->
  TxOut era ->
  TxOut era
setMinCoinTxOutInternal f pp = go
  where
    go !txOut =
      let curMinCoin = getMinCoinTxOut pp txOut
          curCoin = txOut ^. coinTxOutL
       in if curCoin `f` curMinCoin
            then txOut
            else go (txOut & coinTxOutL .~ curMinCoin)

-- | Same as `setMinCoinSizedTxOut`, except it doesn't require the size of the
-- TxOut and will recompute it if needed. Initial amount is not important.
setMinCoinTxOut :: EraTxOut era => PParams era -> TxOut era -> TxOut era
setMinCoinTxOut = setMinCoinTxOutInternal (==)

-- | Similar to `setMinCoinTxOut` it will guarantee that the minimum requirement for the
-- output amount is satisified, however it makes it possible to set a higher amount than
-- the minimaly required.
--
-- @
-- > ensureMinCoinTxOut pp (txOut & coinTxOutL .~ zero) == setMinCoinTxOut pp (txOut & coinTxOutL .~ zero)
-- > (ensureMinCoinTxOut pp txOut ^. coinTxOutL) >= (setMinCoinTxOut pp txOut ^. coinTxOutL)
-- @
ensureMinCoinTxOut :: EraTxOut era => PParams era -> TxOut era -> TxOut era
ensureMinCoinTxOut = setMinCoinTxOutInternal (>=)

setMinCoinSizedTxOutInternal ::
  forall era.
  EraTxOut era =>
  (Coin -> Coin -> Bool) ->
  PParams era ->
  Sized (TxOut era) ->
  Sized (TxOut era)
setMinCoinSizedTxOutInternal f pp = go
  where
    version = eraProtVerLow @era
    go !txOut =
      let curMinCoin = getMinCoinSizedTxOut pp txOut
          curCoin = txOut ^. toSizedL version coinTxOutL
       in if curCoin `f` curMinCoin
            then txOut
            else go (txOut & toSizedL version coinTxOutL .~ curMinCoin)

-- | This function will adjust the output's `Coin` value to the smallest amount
-- allowed by the UTXO rule. Initial amount is not important.
setMinCoinSizedTxOut ::
  forall era.
  EraTxOut era =>
  PParams era ->
  Sized (TxOut era) ->
  Sized (TxOut era)
setMinCoinSizedTxOut = setMinCoinSizedTxOutInternal (==)

-- | Similar to `setMinCoinSizedTxOut` it will guarantee that the minimum requirement for the
-- output amount is satisified, however it makes it possible to set a higher amount than
-- the minimaly required.
--
-- `ensureMinCoinSizedTxOut` relates to `setMinCoinSizedTxOut` in the same way that
-- `ensureMinCoinTxOut` relates to `setMinCoinTxOut`.
ensureMinCoinSizedTxOut ::
  forall era.
  EraTxOut era =>
  PParams era ->
  Sized (TxOut era) ->
  Sized (TxOut era)
ensureMinCoinSizedTxOut = setMinCoinSizedTxOutInternal (>=)
