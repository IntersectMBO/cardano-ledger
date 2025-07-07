{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Tx () where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.BlockBody (AlonzoBlockBody (..))
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx,
  AlonzoTx (..),
  auxDataAlonzoTxL,
  bodyAlonzoTxL,
  isValidAlonzoTxL,
  mkBasicAlonzoTx,
  sizeAlonzoTxF,
  witsAlonzoTxL,
 )
import Cardano.Ledger.Conway.Tx (AlonzoEraTx (..), getConwayMinFeeTx)
import Cardano.Ledger.Core (
  EraBlockBody (..),
  EraTx (..),
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxAuxData ()
import Cardano.Ledger.Dijkstra.TxBody ()
import Cardano.Ledger.Dijkstra.TxWits ()
import Lens.Micro

instance EraTx DijkstraEra where
  type Tx DijkstraEra = AlonzoTx DijkstraEra

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

  getMinFeeTx = getConwayMinFeeTx

instance AlonzoEraTx DijkstraEra where
  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance EraBlockBody DijkstraEra where
  type BlockBody DijkstraEra = AlonzoBlockBody DijkstraEra
  txSeqBlockBodyL = txSeqBlockBodyL
  fromTxSeq = (^. txSeqBlockBodyL)
  toTxSeq = AlonzoBlockBody
  hashBlockBody = abbHash
  hashTxSeq = abbHash
  numSegComponents = 4
