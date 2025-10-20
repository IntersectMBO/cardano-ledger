{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Tx (
  DijkstraTx (..),
  Tx (..),
  validateDijkstraNativeScript,
) where

import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..), StrictMaybe)
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx,
  IsValid,
  alonzoTxEqRaw,
 )
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR (..), ToCBOR (..))
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
import Control.DeepSeq (NFData (..), deepseq)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (InspectHeap (..), NoThunks)

data DijkstraTx l era where
  DijkstraTx ::
    { dtBody :: !(TxBody TopTx era)
    , dtWits :: !(TxWits era)
    , dtIsValid :: !IsValid
    , dtAuxData :: !(StrictMaybe (TxAuxData era))
    } ->
    DijkstraTx TopTx era
  DijkstraSubTx ::
    { dstBody :: !(TxBody SubTx era)
    , dstWits :: !(TxWits era)
    , dstAuxData :: !(StrictMaybe (TxAuxData era))
    } ->
    DijkstraTx SubTx era

deriving instance EraTx era => Eq (DijkstraTx l era)

deriving instance EraTx era => Show (DijkstraTx l era)

instance
  ( EraTx era
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  ) =>
  NFData (DijkstraTx l era)
  where
  rnf DijkstraTx {..} =
    dtBody `deepseq`
      dtWits `deepseq`
        dtIsValid `deepseq`
          rnf dtAuxData
  rnf DijkstraSubTx {..} =
    dstBody `deepseq`
      dstWits `deepseq`
        rnf dstAuxData

deriving via
  InspectHeap (DijkstraTx l era)
  instance
    ( Era era
    , Typeable l
    ) =>
    NoThunks (DijkstraTx l era)

instance (EraTx era, Typeable l) => ToCBOR (DijkstraTx l era) where
  toCBOR = undefined

instance EncCBOR (DijkstraTx l era) where
  encCBOR = undefined

instance (EraTx era, Typeable l) => DecCBOR (Annotator (DijkstraTx l era)) where
  decCBOR = undefined

instance HasEraTxLevel Tx DijkstraEra where
  toSTxLevel (MkDijkstraTx _) = undefined

mkBasicDijkstraTx :: TxBody l DijkstraEra -> DijkstraTx l DijkstraEra
mkBasicDijkstraTx = undefined

instance EraTx DijkstraEra where
  newtype Tx l DijkstraEra = MkDijkstraTx {unDijkstraTx :: DijkstraTx l DijkstraEra}
    deriving newtype (Eq, Show, NFData, NoThunks, ToCBOR, EncCBOR)
    deriving (Generic)

  mkBasicTx = MkDijkstraTx . mkBasicDijkstraTx

  bodyTxL = dijkstraTxL . bodyDijkstraTxL
  {-# INLINE bodyTxL #-}

  witsTxL = dijkstraTxL . witsDijkstraTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = dijkstraTxL . auxDataDijkstraTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = dijkstraTxL . sizeDijkstraTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateDijkstraNativeScript
  {-# INLINE validateNativeScript #-}

  getMinFeeTx = getConwayMinFeeTx

bodyDijkstraTxL :: Lens' (DijkstraTx l era) (TxBody l era)
bodyDijkstraTxL = undefined

witsDijkstraTxL :: Lens' (DijkstraTx l era) (TxWits era)
witsDijkstraTxL = undefined

auxDataDijkstraTxL :: Lens' (DijkstraTx l era) (StrictMaybe (TxAuxData era))
auxDataDijkstraTxL = undefined

sizeDijkstraTxF :: Lens' (DijkstraTx l era) Word32
sizeDijkstraTxF = undefined

instance EqRaw (Tx l DijkstraEra) where
  eqRaw = alonzoTxEqRaw

dijkstraTxL :: Lens' (Tx l DijkstraEra) (DijkstraTx l DijkstraEra)
dijkstraTxL = lens unDijkstraTx (\x y -> x {unDijkstraTx = y})

instance AlonzoEraTx DijkstraEra where
  isValidTxL = undefined -- dijkstraTxL . isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance Typeable l => DecCBOR (Annotator (Tx l DijkstraEra)) where
  decCBOR = fmap MkDijkstraTx <$> decCBOR

validateDijkstraNativeScript ::
  ( EraTx era
  , DijkstraEraTxBody era
  , DijkstraEraScript era
  , NativeScript era ~ DijkstraNativeScript era
  ) =>
  Tx l era -> NativeScript era -> Bool
validateDijkstraNativeScript tx =
  evalDijkstraNativeScript vhks (tx ^. bodyTxL . vldtTxBodyL) (tx ^. bodyTxL . guardsTxBodyL)
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
{-# INLINEABLE validateDijkstraNativeScript #-}
