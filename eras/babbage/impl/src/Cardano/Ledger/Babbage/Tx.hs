{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Tx (
  AlonzoTx (..),
  TxBody (..),
  module X,
) where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Tx as X
import Cardano.Ledger.Alonzo.TxSeq (
  AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
  hashAlonzoTxSeq,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxAuxData ()
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxBodyUpgradeError,
  TxBody (..),
 )
import Cardano.Ledger.Babbage.TxWits ()
import Cardano.Ledger.Core
import Control.Arrow (left)

newtype BabbageTxUpgradeError
  = BTUEBodyUpgradeError BabbageTxBodyUpgradeError
  deriving (Eq, Show)

instance EraTx BabbageEra where
  type Tx BabbageEra = AlonzoTx BabbageEra
  type TxUpgradeError BabbageEra = BabbageTxUpgradeError

  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  wireSizeTxF = wireSizeAlonzoTxF
  {-# INLINE wireSizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = alonzoMinFeeTx pp tx

  upgradeTx (AlonzoTx b w valid aux) =
    AlonzoTx
      <$> left BTUEBodyUpgradeError (upgradeTxBody b)
      <*> pure (upgradeTxWits w)
      <*> pure valid
      <*> pure (fmap upgradeTxAuxData aux)

instance AlonzoEraTx BabbageEra where
  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance EraSegWits BabbageEra where
  type TxSeq BabbageEra = AlonzoTxSeq BabbageEra
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4
