{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Tx (Tx (..)) where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx,
  AlonzoTx (..),
  alonzoTxEqRaw,
  auxDataAlonzoTxL,
  bodyAlonzoTxL,
  isValidAlonzoTxL,
  mkBasicAlonzoTx,
  sizeAlonzoTxF,
  witsAlonzoTxL,
 )
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR)
import Cardano.Ledger.Conway.Tx (AlonzoEraTx (..), Tx (..), getConwayMinFeeTx)
import Cardano.Ledger.Core (EraTx (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxAuxData ()
import Cardano.Ledger.Dijkstra.TxBody ()
import Cardano.Ledger.Dijkstra.TxWits ()
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

instance EraTx DijkstraEra where
  newtype Tx DijkstraEra = MkDijkstraTx {unDijkstraTx :: AlonzoTx DijkstraEra}
    deriving newtype (Eq, Show, NFData, NoThunks, ToCBOR, EncCBOR)
    deriving (Generic)

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

instance EqRaw (Tx DijkstraEra) where
  eqRaw = alonzoTxEqRaw

dijkstraTxL :: Lens' (Tx DijkstraEra) (AlonzoTx DijkstraEra)
dijkstraTxL = lens unDijkstraTx (\x y -> x {unDijkstraTx = y})

instance AlonzoEraTx DijkstraEra where
  isValidTxL = dijkstraTxL . isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance DecCBOR (Annotator (Tx DijkstraEra)) where
  decCBOR = fmap MkDijkstraTx <$> decCBOR
