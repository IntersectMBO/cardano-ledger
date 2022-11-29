{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Tx
  ( module BabbageTxReExport,
  )
where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Tx
  ( alonzoMinFeeTx,
    auxDataAlonzoTxL,
    bodyAlonzoTxL,
    isValidAlonzoTxL,
    mkBasicAlonzoTx,
    sizeAlonzoTxF,
    witsAlonzoTxL,
  )
import Cardano.Ledger.Alonzo.TxSeq
  ( AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
    hashAlonzoTxSeq,
  )
import Cardano.Ledger.Babbage.Tx as BabbageTxReExport
  ( AlonzoEraTx (..),
    AlonzoTx (..),
  )
import Cardano.Ledger.Conway.Era (ConwayEra)
import qualified Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Conway.TxWits ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC

instance CC.Crypto c => EraTx (ConwayEra c) where
  {-# SPECIALIZE instance EraTx (ConwayEra CC.StandardCrypto) #-}

  type Tx (ConwayEra c) = AlonzoTx (ConwayEra c)

  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateScript (Phase1Script script) tx = validateTimelock @(ConwayEra c) script tx
  {-# INLINE validateScript #-}

  getMinFeeTx = alonzoMinFeeTx

instance CC.Crypto c => AlonzoEraTx (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTx (ConwayEra CC.StandardCrypto) #-}

  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance CC.Crypto c => EraSegWits (ConwayEra c) where
  type TxSeq (ConwayEra c) = AlonzoTxSeq (ConwayEra c)
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4
