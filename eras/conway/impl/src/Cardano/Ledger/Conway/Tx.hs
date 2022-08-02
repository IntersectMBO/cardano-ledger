{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Tx
  ( module BabbageTxReExport,
  )
where

import Cardano.Ledger.Alonzo.Tx
  ( auxDataAlonzoTxL,
    bodyAlonzoTxL,
    isValidAlonzoTxL,
    mkBasicAlonzoTx,
    sizeAlonzoTxF,
    validateAlonzoNativeScript,
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
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Conway.TxWits ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC

instance CC.Crypto c => EraTx (ConwayEra c) where
  type Tx (ConwayEra c) = AlonzoTx (ConwayEra c)
  mkBasicTx = mkBasicAlonzoTx
  bodyTxL = bodyAlonzoTxL
  witsTxL = witsAlonzoTxL
  auxDataTxL = auxDataAlonzoTxL
  sizeTxF = sizeAlonzoTxF
  validateScript = validateAlonzoNativeScript

instance CC.Crypto c => AlonzoEraTx (ConwayEra c) where
  isValidTxL = isValidAlonzoTxL

instance CC.Crypto c => EraSegWits (ConwayEra c) where
  type TxSeq (ConwayEra c) = AlonzoTxSeq (ConwayEra c)
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4
