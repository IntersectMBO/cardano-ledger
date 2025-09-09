{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Tx (
  Tx (..),
  validateDijkstraNativeScript,
) where

import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..))
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
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (
  DijkstraEraScript,
  DijkstraNativeScript,
  evalDijkstraNativeScript,
 )
import Cardano.Ledger.Dijkstra.TxAuxData ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Dijkstra.TxWits ()
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Control.DeepSeq (NFData)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
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

  validateNativeScript = validateDijkstraNativeScript
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

validateDijkstraNativeScript ::
  ( EraTx era
  , DijkstraEraTxBody era
  , DijkstraEraScript era
  , NativeScript era ~ DijkstraNativeScript era
  ) =>
  Tx era -> NativeScript era -> Bool
validateDijkstraNativeScript tx =
  evalDijkstraNativeScript vhks (tx ^. bodyTxL . vldtTxBodyL) (tx ^. bodyTxL . guardsTxBodyL)
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
{-# INLINEABLE validateDijkstraNativeScript #-}
