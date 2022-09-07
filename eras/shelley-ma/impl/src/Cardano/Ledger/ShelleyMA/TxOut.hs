{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.TxOut (scaledMinDeposit) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (TxBody)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.PParams (_minUTxOValue)
import Cardano.Ledger.Shelley.TxBody
  ( ShelleyTxOut (..),
    addrEitherShelleyTxOutL,
    valueEitherShelleyTxOutL,
  )
import Cardano.Ledger.ShelleyMA.Era
  ( MAClass,
    MaryOrAllegra (..),
    ShelleyMAEra,
  )
import Cardano.Ledger.Val
  ( Val (isAdaOnly, size),
  )
import Lens.Micro

instance MAClass ma crypto => EraTxOut (ShelleyMAEra ma crypto) where
  {-# SPECIALIZE instance EraTxOut (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance EraTxOut (ShelleyMAEra 'Allegra StandardCrypto) #-}

  type TxOut (ShelleyMAEra ma crypto) = ShelleyTxOut (ShelleyMAEra ma crypto)

  mkBasicTxOut = ShelleyTxOut

  addrEitherTxOutL = addrEitherShelleyTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherShelleyTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinTxOut pp txOut = scaledMinDeposit (txOut ^. valueTxOutL) (_minUTxOValue pp)

-- | The `scaledMinDeposit` calculation uses the minUTxOValue protocol parameter
-- (passed to it as Coin mv) as a specification of "the cost of making a
-- Shelley-sized UTxO entry", calculated here by "utxoEntrySizeWithoutVal +
-- uint", using the constants in the "where" clause.  In the case when a UTxO
-- entry contains coins only (and the Shelley UTxO entry format is used - we
-- will extend this to be correct for other UTxO formats shortly), the deposit
-- should be exactly the minUTxOValue.  This is the "inject (coin v) == v" case.
-- Otherwise, we calculate the per-byte deposit by multiplying the minimum
-- deposit (which is for the number of Shelley UTxO-entry bytes) by the size of
-- a Shelley UTxO entry.  This is the "(mv * (utxoEntrySizeWithoutVal + uint))"
-- calculation.  We then calculate the total deposit required for making a UTxO
-- entry with a Val-class member v by dividing "(mv * (utxoEntrySizeWithoutVal +
-- uint))" by the estimated total size of the UTxO entry containing v, ie by
-- "(utxoEntrySizeWithoutVal + size v)".  See the formal specification for
-- details.
--
-- This scaling function is right for UTxO, not EUTxO
scaledMinDeposit :: Val v => v -> Coin -> Coin
scaledMinDeposit v (Coin mv)
  | isAdaOnly v = Coin mv -- without non-Coin assets, scaled deposit should be exactly minUTxOValue
  -- The calculation should represent this equation
  -- minValueParameter / coinUTxOSize = actualMinValue / valueUTxOSize
  -- actualMinValue = (minValueParameter / coinUTxOSize) * valueUTxOSize
  | otherwise = Coin $ max mv (coinsPerUTxOWord * (utxoEntrySizeWithoutVal + size v))
  where
    -- lengths obtained from tracing on HeapWords of inputs and outputs
    -- obtained experimentally, and number used here
    -- units are Word64s
    txoutLenNoVal = 14
    txinLen = 7

    -- unpacked CompactCoin Word64 size in Word64s
    coinSize :: Integer
    coinSize = 0

    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = 6 + txoutLenNoVal + txinLen

    -- how much ada does a Word64 of UTxO space cost, calculated from minAdaValue PP
    -- round down
    coinsPerUTxOWord :: Integer
    coinsPerUTxOWord = quot mv (utxoEntrySizeWithoutVal + coinSize)
