{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
  IsValid (..),
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (..), integralToBounded)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBOR (..),
  Encoding,
  ToCBOR (..),
  decodeNullStrictMaybe,
  encodeListLen,
  encodeNullStrictMaybe,
  serialize,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<*!))
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
import Cardano.Ledger.Shelley.Tx (shelleyTxEqRaw)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Trans.Fail.String (errorFail)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
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
  toCBOR = toEraCBOR @era

instance EraTx era => EncCBOR (DijkstraTx l era) where
  encCBOR = toCBORForMempoolSubmission

instance (EraTx era, Typeable l) => DecCBOR (Annotator (DijkstraTx l era)) where
  decCBOR = withSTxBothLevels @l $ \case
    STopTx ->
      decode $
        Ann (RecD DijkstraTx)
          <*! From
          <*! From
          <*! Ann From
          <*! D (sequence <$> decodeNullStrictMaybe decCBOR)
    SSubTx ->
      decode $
        Ann (RecD DijkstraSubTx)
          <*! From
          <*! From
          <*! D (sequence <$> decodeNullStrictMaybe decCBOR)

instance HasEraTxLevel DijkstraTx DijkstraEra where
  toSTxLevel DijkstraTx {} = STopTx
  toSTxLevel DijkstraSubTx {} = SSubTx

instance HasEraTxLevel Tx DijkstraEra where
  toSTxLevel = toSTxLevel . unDijkstraTx

mkBasicDijkstraTx :: TxBody l DijkstraEra -> DijkstraTx l DijkstraEra
mkBasicDijkstraTx txBody =
  case toSTxLevel txBody of
    STopTx ->
      DijkstraTx
        txBody
        mempty
        (IsValid True)
        SNothing
    SSubTx ->
      DijkstraSubTx
        txBody
        mempty
        SNothing

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
bodyDijkstraTxL =
  lens
    ( \case
        DijkstraTx {dtBody} -> dtBody
        DijkstraSubTx {dstBody} -> dstBody
    )
    ( \case
        tx@DijkstraTx {} -> \x -> tx {dtBody = x}
        tx@DijkstraSubTx {} -> \x -> tx {dstBody = x}
    )

witsDijkstraTxL :: Lens' (DijkstraTx l era) (TxWits era)
witsDijkstraTxL =
  lens
    ( \case
        DijkstraTx {dtWits} -> dtWits
        DijkstraSubTx {dstWits} -> dstWits
    )
    ( \case
        tx@DijkstraTx {} -> \x -> tx {dtWits = x}
        tx@DijkstraSubTx {} -> \x -> tx {dstWits = x}
    )

isValidDijkstraTxL :: Lens' (DijkstraTx TopTx era) IsValid
isValidDijkstraTxL =
  lens (\DijkstraTx {dtIsValid} -> dtIsValid) $ \tx txIsValid ->
    case tx of
      DijkstraTx {} -> tx {dtIsValid = txIsValid}

auxDataDijkstraTxL :: Lens' (DijkstraTx l era) (StrictMaybe (TxAuxData era))
auxDataDijkstraTxL =
  lens
    ( \case
        DijkstraTx {dtAuxData} -> dtAuxData
        DijkstraSubTx {dstAuxData} -> dstAuxData
    )
    ( \case
        tx@DijkstraTx {} -> \x -> tx {dtAuxData = x}
        tx@DijkstraSubTx {} -> \x -> tx {dstAuxData = x}
    )

toCBORForSizeComputation ::
  ( EncCBOR (TxBody l era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  DijkstraTx l era ->
  Encoding
toCBORForSizeComputation tx =
  encodeListLen 3
    <> encCBOR (tx ^. bodyDijkstraTxL)
    <> encCBOR (tx ^. witsDijkstraTxL)
    <> encodeNullStrictMaybe encCBOR (tx ^. auxDataDijkstraTxL)

sizeDijkstraTxF ::
  forall era l.
  EraTx era =>
  SimpleGetter (DijkstraTx l era) Word32
sizeDijkstraTxF =
  to $
    errorFail
      . integralToBounded @Int64 @Word32
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation

dijkstraTxEqRaw ::
  ( STxLevel l era ~ STxBothLevels l era
  , AlonzoEraTx era
  ) =>
  Tx l era ->
  Tx l era ->
  Bool
dijkstraTxEqRaw tx1 tx2 =
  shelleyTxEqRaw tx1 tx2
    && withBothTxLevels
      tx1
      ( \tx1' ->
          withBothTxLevels
            tx2
            (\tx2' -> tx1' ^. isValidTxL == tx2' ^. isValidTxL)
            (const True)
      )
      (const True)

instance EqRaw (Tx l DijkstraEra) where
  eqRaw = dijkstraTxEqRaw

dijkstraTxL :: Lens' (Tx l DijkstraEra) (DijkstraTx l DijkstraEra)
dijkstraTxL = lens unDijkstraTx (\x y -> x {unDijkstraTx = y})

instance AlonzoEraTx DijkstraEra where
  isValidTxL = dijkstraTxL . isValidDijkstraTxL
  {-# INLINE isValidTxL #-}

instance Typeable l => DecCBOR (Annotator (Tx l DijkstraEra)) where
  decCBOR = fmap MkDijkstraTx <$> decCBOR

validateDijkstraNativeScript ::
  ( DijkstraEraTxBody era
  , DijkstraEraScript era
  , NativeScript era ~ DijkstraNativeScript era
  ) =>
  Tx l era -> NativeScript era -> Bool
validateDijkstraNativeScript tx =
  evalDijkstraNativeScript vhks (tx ^. bodyTxL . vldtTxBodyL) (tx ^. bodyTxL . guardsTxBodyL)
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
{-# INLINEABLE validateDijkstraNativeScript #-}

--------------------------------------------------------------------------------
-- Mempool Serialisation
--
-- We do not store the Tx bytes for the following reasons:
-- - A Tx serialised in this way never forms part of any hashed structure, hence
--   we do not worry about the serialisation changing and thus seeing a new
--   hash.
-- - The three principal components of this Tx already store their own bytes;
--   here we simply concatenate them. The final component, `IsValid`, is
--   just a flag and very cheap to serialise.
--------------------------------------------------------------------------------

-- | Encode to CBOR for the purposes of transmission from node to node, or from
-- wallet to node.
--
-- Note that this serialisation is neither the serialisation used on-chain
-- (where Txs are deconstructed using segwit), nor the serialisation used for
-- computing the transaction size (which omits the `IsValid` field for
-- compatibility with Mary - see 'toCBORForSizeComputation').
toCBORForMempoolSubmission ::
  ( EncCBOR (TxBody l era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  DijkstraTx l era ->
  Encoding
toCBORForMempoolSubmission = \case
  DijkstraTx {dtBody, dtWits, dtAuxData, dtIsValid} ->
    encode $
      Rec DijkstraTx
        !> To dtBody
        !> To dtWits
        !> To dtIsValid
        !> E (encodeNullStrictMaybe encCBOR) dtAuxData
  DijkstraSubTx {dstBody, dstWits, dstAuxData} ->
    encode $
      Rec DijkstraSubTx
        !> To dstBody
        !> To dstWits
        !> E (encodeNullStrictMaybe encCBOR) dstAuxData
