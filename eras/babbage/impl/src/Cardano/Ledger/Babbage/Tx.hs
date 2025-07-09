{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Tx (
  AlonzoTx (..),
  TxBody (..),
  module X,
) where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.BlockBody (AlonzoBlockBody)
import Cardano.Ledger.Alonzo.Tx as X
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxAuxData ()
import Cardano.Ledger.Babbage.TxBody (
  TxBody (..),
 )
import Cardano.Ledger.Babbage.TxWits ()
import Cardano.Ledger.Core

instance EraTx BabbageEra where
  type Tx BabbageEra = AlonzoTx BabbageEra
  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = alonzoMinFeeTx pp tx

instance AlonzoEraTx BabbageEra where
  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance EraBlockBody BabbageEra where
  type BlockBody BabbageEra = AlonzoBlockBody BabbageEra
  mkBasicBlockBody = mkBasicBlockBody
  txSeqBlockBodyL = txSeqBlockBodyL
  hashBlockBody = hashBlockBody
  numSegComponents = 4
