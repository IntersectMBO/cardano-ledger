-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.Era (
  Era (..),
  getTxOutAddr,
  getTxOutCompactAddr,
  getTxOutEitherAddr,
  getTxOutBootstrapAddress,
  getAllTxInputs,
  TranslationContext,
  TranslateEra (..),
  translateEra',
  translateEraMaybe,
  -- $segWit
  EraSegWits (..),
)
where

import Cardano.Ledger.Address (Addr, BootstrapAddress, CompactAddr)
import Cardano.Ledger.Core
import Cardano.Ledger.TxIn (TxIn)
import Data.Set (Set)
import Lens.Micro

-- | The validity of any individual block depends only on a subset
-- of the UTxO stored in the ledger state. The consensus layer makes
-- use of this fact, and uses the function below to to retrieve the
-- needed UTxO from disk and present only those to the ledger.
-- It is therefore neccessary that this function account for all the
-- different types of inputs inside a transaction.
getAllTxInputs :: EraTxBody era => TxBody era -> Set (TxIn (EraCrypto era))
getAllTxInputs txBody = txBody ^. allInputsTxBodyF

getTxOutEitherAddr ::
  EraTxOut era =>
  TxOut era ->
  Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era))
getTxOutEitherAddr txOut = txOut ^. addrEitherTxOutL
{-# DEPRECATED getTxOutEitherAddr "In favor of `addrEitherTxOutL`" #-}

getTxOutAddr ::
  EraTxOut era =>
  TxOut era ->
  Addr (EraCrypto era)
getTxOutAddr txOut = txOut ^. addrTxOutL
{-# DEPRECATED getTxOutAddr "In favor of `addrTxOutL`" #-}

getTxOutCompactAddr ::
  EraTxOut era =>
  TxOut era ->
  CompactAddr (EraCrypto era)
getTxOutCompactAddr txOut = txOut ^. compactAddrTxOutL
{-# DEPRECATED getTxOutCompactAddr "In favor of `compactAddrTxOutL`" #-}

-- | Get the Bootsrap address from the TxOut. Returns `Nothing` if it is a
-- Shelley address or newer
getTxOutBootstrapAddress ::
  EraTxOut era =>
  TxOut era ->
  Maybe (BootstrapAddress (EraCrypto era))
getTxOutBootstrapAddress txOut = txOut ^. bootAddrTxOutF
{-# DEPRECATED getTxOutBootstrapAddress "In favor of `bootAddrTxOutF`" #-}
