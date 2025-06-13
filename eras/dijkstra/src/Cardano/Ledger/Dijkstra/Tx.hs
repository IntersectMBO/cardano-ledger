{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Ledger.Dijkstra.Tx (Tx (..)) where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
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
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq (..), hashAlonzoTxSeq)
import Cardano.Ledger.Conway.Tx (AlonzoEraTx (..), Tx (..), getConwayMinFeeTx)
import Cardano.Ledger.Core (
  EraSegWits (..),
  EraTx (..),
  EraTxAuxData (..),
  EraTxBody (..),
  EraTxWits (..),
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxAuxData ()
import Cardano.Ledger.Dijkstra.TxBody ()
import Cardano.Ledger.Dijkstra.TxWits ()
import Lens.Micro (Lens', lens)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import NoThunks.Class (NoThunks)
import Cardano.Ledger.Binary (ToCBOR, EncCBOR, Annotator, DecCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode(..), decode, (<*!))

instance EraTx DijkstraEra where
  newtype Tx DijkstraEra = MkDijkstraTx {unDijkstraTx :: AlonzoTx DijkstraEra}
    deriving newtype (Eq, Show, NFData, NoThunks, ToCBOR, EncCBOR)
    deriving Generic
  type TxUpgradeError DijkstraEra = TxBodyUpgradeError DijkstraEra

  mkBasicTx = MkDijkstraTx . mkBasicAlonzoTx

  bodyTxL = dijkstraTxL . bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = dijkstraTxL . witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = dijkstraTxL . auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = dijkstraTxL . sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx = getConwayMinFeeTx

  upgradeTx (MkConwayTx (AlonzoTx b w valid aux)) =
    fmap MkDijkstraTx $
      AlonzoTx
        <$> upgradeTxBody b
        <*> pure (upgradeTxWits w)
        <*> pure valid
        <*> pure (fmap upgradeTxAuxData aux)

dijkstraTxL :: Lens' (Tx DijkstraEra) (AlonzoTx DijkstraEra)
dijkstraTxL = lens unDijkstraTx (\x y -> x {unDijkstraTx = y})

instance AlonzoEraTx DijkstraEra where
  isValidTxL = dijkstraTxL . isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance EraSegWits DijkstraEra where
  type TxSeq DijkstraEra = AlonzoTxSeq DijkstraEra
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4

instance DecCBOR (Annotator (Tx DijkstraEra)) where
  decCBOR = decode $ Ann (RecD MkDijkstraTx) <*! From
