{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Tx (
  module BabbageTxReExport,
)
where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Tx (
  alonzoMinFeeTx,
  auxDataAlonzoTxL,
  bodyAlonzoTxL,
  isValidAlonzoTxL,
  mkBasicAlonzoTx,
  sizeAlonzoTxF,
  witsAlonzoTxL,
 )
import Cardano.Ledger.Alonzo.TxSeq (
  AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
  hashAlonzoTxSeq,
 )
import Cardano.Ledger.Babbage.Tx as BabbageTxReExport (
  AlonzoEraTx (..),
  AlonzoTx (..),
 )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.TxAuxData ()
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Conway.TxWits ()
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto

instance Crypto c => EraTx (ConwayEra c) where
  {-# SPECIALIZE instance EraTx (ConwayEra StandardCrypto) #-}

  type Tx (ConwayEra c) = AlonzoTx (ConwayEra c)
  type TxUpgradeError (ConwayEra c) = TxBodyUpgradeError (ConwayEra c)

  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateScript (Phase1Script script) = validateTimelock @(ConwayEra c) script
  {-# INLINE validateScript #-}

  getMinFeeTx = alonzoMinFeeTx

  upgradeTx (AlonzoTx b w valid aux) =
    AlonzoTx
      <$> upgradeTxBody b
      <*> pure (upgradeTxWits w)
      <*> pure valid
      <*> pure (fmap upgradeTxAuxData aux)

instance Crypto c => AlonzoEraTx (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTx (ConwayEra StandardCrypto) #-}

  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance Crypto c => EraSegWits (ConwayEra c) where
  type TxSeq (ConwayEra c) = AlonzoTxSeq (ConwayEra c)
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4
