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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Tx (
  DijkstraTx (..),
  DijkstraTopTx (..),
  DijkstraTopTxRaw (..),
  Tx (..),
  DijkstraStAnnTx (..),
  validateDijkstraNativeScript,
  decodeDijkstraTopTxInMempool,
  decodeDijkstraTopTxInBlock,
  toCBORForBlockInclusion,
) where

import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..), StrictMaybe)
import Cardano.Ledger.Alonzo.Plutus.Context (
  CollectError,
  SupportedPlutusRunnable,
  TxInfoResult,
 )
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx,
  IsPhase2Valid (..),
 )
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), unRedeemersL)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), ToKeyValuePairs (..), integralToBounded)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  Encoding,
  ToCBOR (..),
  TokenType (..),
  decodeNullStrictMaybe,
  decodeRecordNamed,
  encodeListLen,
  encodeNullStrictMaybe,
  encodePreEncoded,
  peekTokenType,
  serialize,
 )
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Conway.Tx (AlonzoEraTx (..), Tx (..), getConwayMinFeeTx)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (
  DijkstraEraScript,
  DijkstraNativeScript,
  evalDijkstraNativeScript,
 )
import Cardano.Ledger.Dijkstra.TxAuxData ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..), dijkstraAllInputsTxBodyF)
import Cardano.Ledger.Dijkstra.TxWits ()
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (UnsafeMkMempoolTx), unwrapMempoolTx)
import Cardano.Ledger.Plutus (Language, PlutusWithContext)
import Cardano.Ledger.Shelley.Tx (shelleyTxEqRaw)
import Cardano.Ledger.State
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, lens, to, (&), (.~), (^.))
import NoThunks.Class (InspectHeap (..), NoThunks)

data DijkstraTopTxRaw era = DijkstraTopTxRaw
  { dttrBody :: !(TxBody TopTx era)
  , dttrWits :: !(TxWits era)
  , dttrIsPhase2Valid :: !IsPhase2Valid
  , dttrAuxData :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic)

deriving instance EraTx era => Eq (DijkstraTopTxRaw era)

deriving instance EraTx era => Show (DijkstraTopTxRaw era)

instance EraTx era => NFData (DijkstraTopTxRaw era)

type instance MemoHashIndex (DijkstraTopTxRaw era) = EraIndependentTx

-- | Encode in the format used inside a block body:
-- @[transaction_body, transaction_witness_set, auxiliary_data\/ nil, is_valid]@.
--
-- The `IsPhase2Valid` flag is placed after the fields supplied by the
-- transaction author, since it is set by the block producer.
instance EraTx era => EncCBOR (DijkstraTopTxRaw era) where
  encCBOR DijkstraTopTxRaw {dttrBody, dttrWits, dttrIsPhase2Valid, dttrAuxData} =
    encodeListLen 4
      <> encCBOR dttrBody
      <> encCBOR dttrWits
      <> encodeNullStrictMaybe encCBOR dttrAuxData
      <> encCBOR dttrIsPhase2Valid

instance EraTx era => DecCBOR (Annotator (DijkstraTopTxRaw era)) where
  decCBOR =
    decodeRecordNamed "DijkstraTx" (const 4) $ do
      bodyAnn <- decCBOR
      witsAnn <- decCBOR
      auxAnn <- decodeNullStrictMaybe decCBOR
      isPhase2Valid <- decCBOR
      pure $ DijkstraTopTxRaw <$> bodyAnn <*> witsAnn <*> pure isPhase2Valid <*> sequence auxAnn

-- Note: 'EncCBOR' for 'DijkstraTopTx', as well as for a top-level 'DijkstraTx'
-- and 'Tx', emits the stored block-format bytes. The mempool format is only
-- produced for a 'MempoolTx' (see 'toCBORForMempoolSubmission')
newtype DijkstraTopTx era = MkDijkstraTopTx (MemoBytes (DijkstraTopTxRaw era))
  deriving newtype (SafeToHash, EncCBOR)
  deriving (Generic)

deriving newtype instance EraTx era => Eq (DijkstraTopTx era)

deriving newtype instance EraTx era => Show (DijkstraTopTx era)

deriving newtype instance EraTx era => NFData (DijkstraTopTx era)

instance Memoized (DijkstraTopTx era) where
  type RawType (DijkstraTopTx era) = DijkstraTopTxRaw era

instance HashAnnotated (DijkstraTopTx era) EraIndependentTx where
  hashAnnotated = getMemoSafeHash

deriving via
  Mem (DijkstraTopTxRaw era)
  instance
    EraTx era => DecCBOR (Annotator (DijkstraTopTx era))

bodyDijkstraTopTxL :: forall era. EraTx era => Lens' (DijkstraTopTx era) (TxBody TopTx era)
bodyDijkstraTopTxL = lensMemoRawType @era dttrBody $ \r x -> r {dttrBody = x}

witsDijkstraTopTxL :: forall era. EraTx era => Lens' (DijkstraTopTx era) (TxWits era)
witsDijkstraTopTxL = lensMemoRawType @era dttrWits $ \r x -> r {dttrWits = x}

isPhase2ValidDijkstraTopTxL :: forall era. EraTx era => Lens' (DijkstraTopTx era) IsPhase2Valid
isPhase2ValidDijkstraTopTxL =
  lensMemoRawType @era dttrIsPhase2Valid $ \r x -> r {dttrIsPhase2Valid = x}

auxDataDijkstraTopTxL ::
  forall era. EraTx era => Lens' (DijkstraTopTx era) (StrictMaybe (TxAuxData era))
auxDataDijkstraTopTxL = lensMemoRawType @era dttrAuxData $ \r x -> r {dttrAuxData = x}

data DijkstraTx l era where
  DijkstraTopTx :: !(DijkstraTopTx era) -> DijkstraTx TopTx era
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
  rnf (DijkstraTopTx t) = rnf t
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

-- | A top-level transaction is encoded in the block format, while a sub-transaction is
-- encoded as @[transaction_body, transaction_witness_set, auxiliary_data\/ nil]@.
instance EraTx era => EncCBOR (DijkstraTx l era) where
  encCBOR = \case
    DijkstraTopTx t -> encCBOR t
    DijkstraSubTx {dstBody, dstWits, dstAuxData} ->
      encodeListLen 3
        <> encCBOR dstBody
        <> encCBOR dstWits
        <> encodeNullStrictMaybe encCBOR dstAuxData

instance SafeToHash (DijkstraTx TopTx era) where
  originalBytes (DijkstraTopTx t) = originalBytes t
  originalBytesSize (DijkstraTopTx t) = originalBytesSize t

instance HashAnnotated (DijkstraTx TopTx era) EraIndependentTx where
  hashAnnotated (DijkstraTopTx t) = getMemoSafeHash t

-- | Decode a top-level transaction in the mempool format:
-- @[transaction_body, transaction_witness_set, auxiliary_data\/ nil]@. For
-- backwards compatibility the legacy
-- @[transaction_body, transaction_witness_set, true, auxiliary_data\/ nil]@
-- format is also accepted, but only with the `IsPhase2Valid` flag set to `True`.
decodeDijkstraTopTxInMempool ::
  forall era s. EraTx era => Decoder s (Annotator (DijkstraTx TopTx era))
decodeDijkstraTopTxInMempool =
  fmap snd $ decodeRecordNamed "DijkstraTx" fst $ do
    bodyAnn <- decCBOR
    witsAnn <- decCBOR
    isValidFlagSupplied <-
      peekTokenType >>= \case
        TypeBool ->
          decCBOR >>= \case
            True -> pure True
            False -> fail "Value `false` not allowed for `isPhase2Valid`"
        _ -> pure False
    auxAnn <- decodeNullStrictMaybe decCBOR
    let
      -- `isValid == False` can no longer be supplied in an encoded transaction.
      dijkstraTopTx =
        DijkstraTopTx . mkMemoizedEra @era
          <$> (DijkstraTopTxRaw <$> bodyAnn <*> witsAnn <*> pure Phase2Valid <*> sequence auxAnn)
    pure (if isValidFlagSupplied then 4 else 3, dijkstraTopTx)

-- | Decode a top-level transaction in the format used inside a block body:
-- @[transaction_body, transaction_witness_set, auxiliary_data\/ nil, is_valid]@.
-- Unlike in the mempool format, the trailing `IsPhase2Valid` flag is mandatory
-- and can be `False`, since it is set by the block producer.
decodeDijkstraTopTxInBlock :: EraTx era => Decoder s (Annotator (DijkstraTx TopTx era))
decodeDijkstraTopTxInBlock = fmap DijkstraTopTx <$> decCBOR

instance (EraTx era, Typeable l) => DecCBOR (Annotator (DijkstraTx l era)) where
  decCBOR = withSTxBothLevels @l $ \case
    STopTx -> decodeDijkstraTopTxInBlock
    SSubTx ->
      decodeRecordNamed "DijkstraSubTx" (const 3) $ do
        body <- decCBOR
        wits <- decCBOR
        aux <- sequence <$> decodeNullStrictMaybe decCBOR
        pure $ DijkstraSubTx <$> body <*> wits <*> aux

instance HasEraTxLevel DijkstraTx DijkstraEra where
  toSTxLevel DijkstraTopTx {} = STopTx
  toSTxLevel DijkstraSubTx {} = SSubTx

instance HasEraTxLevel Tx DijkstraEra where
  toSTxLevel = toSTxLevel . unDijkstraTx

mkBasicDijkstraTx :: TxBody l DijkstraEra -> DijkstraTx l DijkstraEra
mkBasicDijkstraTx txBody =
  case toSTxLevel txBody of
    STopTx ->
      DijkstraTopTx . mkMemoizedEra @DijkstraEra $
        DijkstraTopTxRaw
          txBody
          mempty
          Phase2Valid
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

  type StAnnTx l DijkstraEra = DijkstraStAnnTx l DijkstraEra

  type StAnnTxCache DijkstraEra = Map.Map ScriptHash (SupportedPlutusRunnable DijkstraEra)

  txStAnnTxG = to $ \case
    DijkstraStAnnTopTx {dsattTx} -> dsattTx
    DijkstraStAnnSubTx {dsastTx} -> dsastTx

  cacheStAnnTxG = to $ \case
    DijkstraStAnnTopTx {dsattPlutusRunnableCache} -> dsattPlutusRunnableCache
    DijkstraStAnnSubTx {dsastPlutusRunnableCache} -> dsastPlutusRunnableCache

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

  allInputsTxBodyF = dijkstraAllInputsTxBodyF
  {-# INLINE allInputsTxBodyF #-}

bodyDijkstraTxL :: EraTx era => Lens' (DijkstraTx l era) (TxBody l era)
bodyDijkstraTxL =
  lens
    ( \case
        DijkstraTopTx t -> t ^. bodyDijkstraTopTxL
        DijkstraSubTx {dstBody} -> dstBody
    )
    ( \case
        DijkstraTopTx t -> \x -> DijkstraTopTx (t & bodyDijkstraTopTxL .~ x)
        tx@DijkstraSubTx {} -> \x -> tx {dstBody = x}
    )

witsDijkstraTxL :: EraTx era => Lens' (DijkstraTx l era) (TxWits era)
witsDijkstraTxL =
  lens
    ( \case
        DijkstraTopTx t -> t ^. witsDijkstraTopTxL
        DijkstraSubTx {dstWits} -> dstWits
    )
    ( \case
        DijkstraTopTx t -> \x -> DijkstraTopTx (t & witsDijkstraTopTxL .~ x)
        tx@DijkstraSubTx {} -> \x -> tx {dstWits = x}
    )

isPhase2ValidDijkstraTxL :: EraTx era => Lens' (DijkstraTx TopTx era) IsPhase2Valid
isPhase2ValidDijkstraTxL =
  lens (\(DijkstraTopTx t) -> t ^. isPhase2ValidDijkstraTopTxL) $ \(DijkstraTopTx t) x ->
    DijkstraTopTx (t & isPhase2ValidDijkstraTopTxL .~ x)

auxDataDijkstraTxL :: EraTx era => Lens' (DijkstraTx l era) (StrictMaybe (TxAuxData era))
auxDataDijkstraTxL =
  lens
    ( \case
        DijkstraTopTx t -> t ^. auxDataDijkstraTopTxL
        DijkstraSubTx {dstAuxData} -> dstAuxData
    )
    ( \case
        DijkstraTopTx t -> \x -> DijkstraTopTx (t & auxDataDijkstraTopTxL .~ x)
        tx@DijkstraSubTx {} -> \x -> tx {dstAuxData = x}
    )

toCBORForSizeComputation :: EraTx era => DijkstraTx l era -> Encoding
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
            (\tx2' -> tx1' ^. isPhase2ValidTxL == tx2' ^. isPhase2ValidTxL)
            (const True)
      )
      (const True)

instance EqRaw (Tx l DijkstraEra) where
  eqRaw = dijkstraTxEqRaw

instance
  ( ToJSON (TxBody TopTx DijkstraEra)
  , ToJSON (TxWits DijkstraEra)
  , ToJSON (TxAuxData DijkstraEra)
  ) =>
  ToKeyValuePairs (DijkstraTx TopTx DijkstraEra)
  where
  toKeyValuePairs
    ( DijkstraTopTx
        (getMemoRawType -> DijkstraTopTxRaw {dttrBody, dttrWits, dttrIsPhase2Valid, dttrAuxData})
      ) =
      [ "body" .= dttrBody
      , "wits" .= dttrWits
      , "isPhase2Valid" .= dttrIsPhase2Valid
      , "auxData" .= dttrAuxData
      ]

instance
  ( ToJSON (TxBody TopTx DijkstraEra)
  , ToJSON (TxWits DijkstraEra)
  , ToJSON (TxAuxData DijkstraEra)
  ) =>
  ToJSON (Tx TopTx DijkstraEra)
  where
  toJSON (MkDijkstraTx tx) = Aeson.object (toKeyValuePairs tx)

instance
  ( FromJSON (TxBody TopTx DijkstraEra)
  , FromJSON (TxWits DijkstraEra)
  , FromJSON (TxAuxData DijkstraEra)
  ) =>
  FromJSON (Tx TopTx DijkstraEra)
  where
  parseJSON = withObject "Tx TopTx DijkstraEra" $ \o ->
    fmap (MkDijkstraTx . DijkstraTopTx . mkMemoizedEra @DijkstraEra) $
      DijkstraTopTxRaw
        <$> o .: "body"
        <*> o .: "wits"
        <*> o .: "isPhase2Valid"
        <*> o .: "auxData"

instance
  ( ToJSON (TxBody SubTx DijkstraEra)
  , ToJSON (TxWits DijkstraEra)
  , ToJSON (TxAuxData DijkstraEra)
  ) =>
  ToJSON (Tx SubTx DijkstraEra)
  where
  toJSON (MkDijkstraTx (DijkstraSubTx {dstBody, dstWits, dstAuxData})) =
    Aeson.object
      [ "dstBody" .= dstBody
      , "dstWits" .= dstWits
      , "dstAuxData" .= dstAuxData
      ]

instance
  ( FromJSON (TxBody SubTx DijkstraEra)
  , FromJSON (TxWits DijkstraEra)
  , FromJSON (TxAuxData DijkstraEra)
  ) =>
  FromJSON (Tx SubTx DijkstraEra)
  where
  parseJSON = withObject "Tx SubTx DijkstraEra" $ \o ->
    fmap MkDijkstraTx $
      DijkstraSubTx
        <$> o .: "dstBody"
        <*> o .: "dstWits"
        <*> o .: "dstAuxData"

dijkstraTxL :: Lens' (Tx l DijkstraEra) (DijkstraTx l DijkstraEra)
dijkstraTxL = lens unDijkstraTx (\x y -> x {unDijkstraTx = y})

instance AlonzoEraTx DijkstraEra where
  isPhase2ValidTxL = dijkstraTxL . isPhase2ValidDijkstraTxL
  {-# INLINE isPhase2ValidTxL #-}

  getTotalExUnits tx =
    foldMap snd (tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL)
      <> withBothTxLevels
        tx
        (\topTx -> foldMap getTotalExUnits (topTx ^. bodyTxL . subTransactionsTxBodyL))
        (const mempty)

instance Typeable l => DecCBOR (Annotator (Tx l DijkstraEra)) where
  decCBOR = fmap MkDijkstraTx <$> decCBOR

instance EncCBOR (MempoolTx DijkstraEra) where
  encCBOR = toCBORForMempoolSubmission . unDijkstraTx . unwrapMempoolTx

instance DecCBOR (Annotator (MempoolTx DijkstraEra)) where
  decCBOR = fmap (UnsafeMkMempoolTx . MkDijkstraTx) <$> decodeDijkstraTopTxInMempool

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

--------------------------------------------------------------------------------
-- Serialisation and hashing
--
-- A top-level transaction has two wire formats:
-- - the mempool format @[body, wits, aux_data/ nil]@, used for transmission
--   from node to node or from wallet to node, which omits the `IsPhase2Valid`
--   flag ('toCBORForMempoolSubmission'), and
-- - the block format @[body, wits, aux_data/ nil, is_valid]@, used for
--   transactions inside a block body, where the trailing `IsPhase2Valid` flag
--   is set by the block producer ('toCBORForBlockInclusion').
--
-- A top-level transaction stores its bytes in the block format (see
-- 'DijkstraTopTx'), because they are hashed: endorser blocks refer to
-- transactions by their 'hashAnnotated', which can be checked against the raw
-- bytes without decoding them.
--
-- The 'EncCBOR' and 'DecCBOR' instances for a top-level 'Tx' use the block
-- format, while the mempool format is only used by the instances for
-- 'MempoolTx'.
--
-- The mempool format is not stored. It is assembled from the components, which
-- already store their own bytes. Sub-transactions do not store their bytes
-- either, since they are part of the body of a top-level transaction.
--------------------------------------------------------------------------------

instance SafeToHash (Tx TopTx DijkstraEra) where
  originalBytes = originalBytes . unDijkstraTx
  originalBytesSize = originalBytesSize . unDijkstraTx

instance HashAnnotated (Tx TopTx DijkstraEra) EraIndependentTx where
  hashAnnotated = hashAnnotated . unDijkstraTx

-- | Encode to CBOR for the purposes of transmission from node to node, or from
-- wallet to node. This is the serialisation of a 'MempoolTx'.
--
-- This differs from the block format, which includes the `IsPhase2Valid` flag
-- as the last element (see 'toCBORForBlockInclusion'). It currently matches
-- the encoding used for fee-size computation (see 'toCBORForSizeComputation').
toCBORForMempoolSubmission ::
  ( EncCBOR (TxBody TopTx era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  DijkstraTx TopTx era ->
  Encoding
toCBORForMempoolSubmission = \case
  DijkstraTopTx
    (getMemoRawType -> DijkstraTopTxRaw {dttrBody, dttrWits, dttrIsPhase2Valid, dttrAuxData}) ->
      encode $
        Rec DijkstraTopTxRaw
          !> To dttrBody
          !> To dttrWits
          !> OmitC dttrIsPhase2Valid
          !> E (encodeNullStrictMaybe encCBOR) dttrAuxData

toCBORForBlockInclusion :: SafeToHash (Tx TopTx era) => Tx TopTx era -> Encoding
toCBORForBlockInclusion = encodePreEncoded . originalBytes

data DijkstraStAnnTx l era where
  DijkstraStAnnTopTx ::
    { dsattTx :: !(Tx TopTx era)
    , dsattScriptsNeeded :: ScriptsNeeded era
    , dsattScriptsProvided :: ScriptsProvided era
    , dsattPlutusLegacyMode :: Bool
    , dsattPlutusLanguagesUsed :: Set Language
    , dsattPlutusRunnableCache :: Map.Map ScriptHash (SupportedPlutusRunnable era)
    , dsattPlutusScriptsWithContext :: Either (NonEmpty (CollectError era)) [PlutusWithContext]
    , dsattSubTransactions :: [DijkstraStAnnTx SubTx era]
    } ->
    DijkstraStAnnTx TopTx era
  DijkstraStAnnSubTx ::
    { dsastTx :: !(Tx SubTx era)
    , dsastScriptsNeeded :: ScriptsNeeded era
    , dsastScriptsHashesNeeded :: Set ScriptHash
    , dsastScriptsProvided :: ScriptsProvided era
    , dsastTxInfoResult :: TxInfoResult era
    , dsastPlutusLanguagesUsed :: Set Language
    , dsastPlutusRunnableCache :: Map.Map ScriptHash (SupportedPlutusRunnable era)
    , dsastPlutusScriptsWithContext :: Either (NonEmpty (CollectError era)) [PlutusWithContext]
    } ->
    DijkstraStAnnTx SubTx era

deriving instance
  ( DijkstraEraScript era
  , Eq (Tx TopTx era)
  , Eq (Tx SubTx era)
  , Eq (ScriptsNeeded era)
  , Eq (ScriptsProvided era)
  , Eq (CollectError era)
  , Eq (TxInfoResult era)
  ) =>
  Eq (DijkstraStAnnTx l era)

deriving instance
  ( DijkstraEraScript era
  , Show (Tx TopTx era)
  , Show (Tx SubTx era)
  , Show (ScriptsNeeded era)
  , Show (ScriptsProvided era)
  , Show (CollectError era)
  , Show (TxInfoResult era)
  ) =>
  Show (DijkstraStAnnTx l era)

instance
  ( EraTxLevel era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  HasEraTxLevel DijkstraStAnnTx era
  where
  toSTxLevel DijkstraStAnnTopTx {} = STopTx @era
  toSTxLevel DijkstraStAnnSubTx {} = SSubTx @era
