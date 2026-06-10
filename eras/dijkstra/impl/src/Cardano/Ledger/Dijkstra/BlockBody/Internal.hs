{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides BlockBody internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.Dijkstra.BlockBody.Internal (
  DijkstraBlockBody (DijkstraBlockBody, DijkstraBlockBodyResolved, ..),
  hashDijkstraSegWits,
  alignedValidFlags,
  mkBasicBlockBodyDijkstra,
  DijkstraEraBlockBody (..),
  LeiosCert,
  encodeLeiosCert,
  decodeLeiosCert,
) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Leios (LeiosCert)
import qualified Cardano.Crypto.Leios as Leios
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.BaseTypes (PerasCert)
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  Decoder,
  EncCBORGroup (..),
  Encoding,
  decodeNullStrictMaybe,
  encCBOR,
  encodeFoldableEncoder,
  encodeFoldableMapEncoder,
  encodeNullStrictMaybe,
  encodePreEncoded,
  fromPlainDecoder,
  fromPlainEncoding,
  serialize,
  withSlice,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Shelley.BlockBody (auxDataSeqDecoder)
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, shortByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (
  StrictMaybe (..),
  maybeToStrictMaybe,
  strictMaybeToMaybe,
 )
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (AllowThunksIn (..), NoThunks)

-- =================================================

-- $BlockBody
--
-- * BlockBody
--
-- BlockBody provides an alternate way of formatting transactions in a block, in
-- order to support segregated witnessing.

data DijkstraBlockBody era = DijkstraBlockBodyInternal
  { dbbTxs :: !(StrictSeq (Tx TopTx era))
  , dbbLeiosCert :: !(StrictMaybe LeiosCert)
  -- ^ Optional Leios certificate. When 'SJust', the EB closure carrying
  -- the actual transactions must be looked up via the LeiosDB.
  , dbbPerasCert :: !(StrictMaybe PerasCert)
  -- ^ Optional Peras certificate
  , dbbHash :: Hash.Hash HASH EraIndependentBlockBody
  -- ^ Memoized hash to avoid recomputation. Lazy on purpose.
  , dbbTxsBodyBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxBody' era)@
  , dbbTxsWitsBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxWits' era)@
  , dbbTxsAuxDataBytes :: BSL.ByteString
  -- ^ Bytes encoding a @'TxAuxData')@. Missing indices have
  -- 'SNothing' for metadata
  , dbbTxsIsValidBytes :: BSL.ByteString
  -- ^ Bytes representing a set of integers. These are the indices of
  -- transactions with 'isValid' == False.
  , dbbLeiosCertBytes :: Maybe BSL.ByteString
  -- ^ Bytes encoding the optional Leios certificate
  , dbbPerasCertBytes :: Maybe BSL.ByteString
  -- ^ Bytes encoding the optional Peras certificate
  }
  deriving (Generic)

instance
  (NFData (Tx TopTx era), NFData LeiosCert, NFData PerasCert) =>
  NFData (DijkstraBlockBody era)

instance EraBlockBody DijkstraEra where
  type BlockBody DijkstraEra = DijkstraBlockBody DijkstraEra
  mkBasicBlockBody = mkBasicBlockBodyDijkstra

  -- Rebuild via the bidirectional pattern so the cached body/wits/aux/isvalid
  -- bytes and the body hash are recomputed for the new tx sequence. Mirrors
  -- the Alonzo idiom 'lens abbTxs (\_ s -> AlonzoBlockBody s)'.
  txSeqBlockBodyL =
    lens dbbTxs (\bb txs -> DijkstraBlockBody txs (dbbLeiosCert bb) (dbbPerasCert bb))
  hashBlockBody = dbbHash
  numSegComponents = 6

mkBasicBlockBodyDijkstra ::
  ( SafeToHash (TxWits era)
  , BlockBody era ~ DijkstraBlockBody era
  , AlonzoEraTx era
  ) =>
  BlockBody era
mkBasicBlockBodyDijkstra = DijkstraBlockBody mempty SNothing SNothing
{-# INLINEABLE mkBasicBlockBodyDijkstra #-}

-- | Dijkstra-specific extensions to 'EraBlockBody'
class EraBlockBody era => DijkstraEraBlockBody era where
  leiosCertBlockBodyL :: Lens' (BlockBody era) (StrictMaybe LeiosCert)
  -- ^ Lens to access the optional Leios certificate in the block body

  perasCertBlockBodyL :: Lens' (BlockBody era) (StrictMaybe PerasCert)
  -- ^ Lens to access the optional Peras certificate in the block body

instance DijkstraEraBlockBody DijkstraEra where
  leiosCertBlockBodyL =
    lens dbbLeiosCert (\bb c -> DijkstraBlockBody (dbbTxs bb) c (dbbPerasCert bb))
  perasCertBlockBodyL =
    lens dbbPerasCert (\bb c -> DijkstraBlockBody (dbbTxs bb) (dbbLeiosCert bb) c)

mkDijkstraBlockBodyInternal ::
  forall era.
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  Bool ->
  StrictSeq (Tx TopTx era) ->
  StrictMaybe LeiosCert ->
  StrictMaybe PerasCert ->
  DijkstraBlockBody era
mkDijkstraBlockBodyInternal keepTxBytes txns mbLeiosCert mbPerasCert =
  let version = eraProtVerLow @era
      serializeFoldablePreEncoded x =
        serialize version $
          encodeFoldableEncoder encodePreEncoded x
      metaChunk index m = encodeIndexed <$> strictMaybeToMaybe m
        where
          encodeIndexed metadata = encCBOR index <> encodePreEncoded metadata
      txsAsBytes =
        ( serializeFoldablePreEncoded $ originalBytes . view bodyTxL <$> txns
        , serializeFoldablePreEncoded $ originalBytes . view witsTxL <$> txns
        , serialize version . encodeFoldableMapEncoder metaChunk $
            fmap originalBytes . view auxDataTxL <$> txns
        , serialize version $ encCBOR $ nonValidatingIndices txns
        )
      emptyTxs =
        ( serializeFoldablePreEncoded (mempty :: StrictSeq ByteString)
        , serializeFoldablePreEncoded (mempty :: StrictSeq ByteString)
        , serialize
            version
            (encodeFoldableMapEncoder metaChunk (mempty :: StrictSeq (StrictMaybe ByteString)))
        , serialize version (encCBOR ([] :: [Int]))
        )
      (txSeqBodies, txSeqWits, txSeqAuxDatas, txSeqIsValids) =
        case mbLeiosCert of
          SJust _ | not keepTxBytes -> emptyTxs
          _ -> txsAsBytes
      mbLeiosCertBytes =
        Just (serialize version (encodeNullStrictMaybe encodeLeiosCert mbLeiosCert))
      mbPerasCertBytes =
        Just (serialize version (encodeNullStrictMaybe encCBOR mbPerasCert))
   in DijkstraBlockBodyInternal
        { dbbTxs = txns
        , dbbLeiosCert = mbLeiosCert
        , dbbPerasCert = mbPerasCert
        , dbbHash =
            hashDijkstraSegWits
              txSeqBodies
              txSeqWits
              txSeqAuxDatas
              txSeqIsValids
              mbLeiosCertBytes
              mbPerasCertBytes
        , dbbTxsBodyBytes = txSeqBodies
        , dbbTxsWitsBytes = txSeqWits
        , dbbTxsAuxDataBytes = txSeqAuxDatas
        , dbbTxsIsValidBytes = txSeqIsValids
        , dbbLeiosCertBytes = mbLeiosCertBytes
        , dbbPerasCertBytes = mbPerasCertBytes
        }

pattern DijkstraBlockBody ::
  forall era.
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx TopTx era) ->
  StrictMaybe LeiosCert ->
  StrictMaybe PerasCert ->
  DijkstraBlockBody era
pattern DijkstraBlockBody xs mbLeiosCert mbPerasCert <-
  DijkstraBlockBodyInternal xs mbLeiosCert mbPerasCert _ _ _ _ _ _ _
  where
    DijkstraBlockBody = mkDijkstraBlockBodyInternal False

{-# COMPLETE DijkstraBlockBody #-}

-- | Like 'DijkstraBlockBody', but keeps the tx segments in the cached body
-- bytes even when a 'LeiosCert' is present. Used by the NTC chainsync
-- server's resolve path.
pattern DijkstraBlockBodyResolved ::
  forall era.
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx TopTx era) ->
  StrictMaybe LeiosCert ->
  StrictMaybe PerasCert ->
  DijkstraBlockBody era
pattern DijkstraBlockBodyResolved xs mbLeiosCert mbPerasCert <-
  DijkstraBlockBodyInternal xs mbLeiosCert mbPerasCert _ _ _ _ _ _ _
  where
    DijkstraBlockBodyResolved = mkDijkstraBlockBodyInternal True

deriving via
  AllowThunksIn
    '[ "dbbHash"
     , "dbbTxsBodyBytes"
     , "dbbTxsWitsBytes"
     , "dbbTxsAuxDataBytes"
     , "dbbTxsIsValidBytes"
     , "dbbLeiosCertBytes"
     , "dbbPerasCertBytes"
     ]
    (DijkstraBlockBody era)
  instance
    (Typeable era, NoThunks (Tx TopTx era)) => NoThunks (DijkstraBlockBody era)

deriving stock instance Show (Tx TopTx era) => Show (DijkstraBlockBody era)

deriving stock instance Eq (Tx TopTx era) => Eq (DijkstraBlockBody era)

--------------------------------------------------------------------------------
-- Serialisation and hashing
--------------------------------------------------------------------------------

instance Era era => EncCBORGroup (DijkstraBlockBody era) where
  encCBORGroup blockBody =
    encodePreEncoded $
      BSL.toStrict $
        dbbTxsBodyBytes blockBody
          <> dbbTxsWitsBytes blockBody
          <> dbbTxsAuxDataBytes blockBody
          <> dbbTxsIsValidBytes blockBody
          <> fromMaybe BSL.empty (dbbLeiosCertBytes blockBody)
          <> fromMaybe BSL.empty (dbbPerasCertBytes blockBody)

  -- The cert byte fields always encode something (a CBOR null when
  -- absent, the cert value when present), so the body always has 6
  -- elements.
  listLen _ = 6

hashDijkstraSegWits ::
  BSL.ByteString ->
  -- | Bytes for transaction bodies
  BSL.ByteString ->
  -- | Bytes for transaction witnesses
  BSL.ByteString ->
  -- | Bytes for transaction auxiliary datas
  BSL.ByteString ->
  -- | Bytes for transaction isValid flags
  Maybe BSL.ByteString ->
  -- | Bytes for optional Leios certificate
  Maybe BSL.ByteString ->
  -- | Bytes for optional Peras certificate
  Hash HASH EraIndependentBlockBody
hashDijkstraSegWits txSeqBodies txSeqWits txAuxData txSeqIsValids mbLeiosCert mbPerasCert =
  coerce . hashLazy . toLazyByteString $
    hashPart txSeqBodies
      <> hashPart txSeqWits
      <> hashPart txAuxData
      <> hashPart txSeqIsValids
      <> maybe mempty hashPart mbLeiosCert
      <> maybe mempty hashPart mbPerasCert
  where
    hashLazy :: BSL.ByteString -> Hash HASH ByteString
    hashLazy = Hash.hashWith id . BSL.toStrict
    {-# INLINE hashLazy #-}
    hashPart :: BSL.ByteString -> Builder
    hashPart = shortByteString . Hash.hashToBytesShort . hashLazy
    {-# INLINE hashPart #-}
{-# INLINE hashDijkstraSegWits #-}

instance
  ( AlonzoEraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody TopTx era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  DecCBOR (Annotator (DijkstraBlockBody era))
  where
  -- The body is encoded as 6 group elements (via 'encCBORGroup'), so
  -- the outer 'decodeRecordNamed \"Block\" (const 7)' in
  -- 'Cardano.Ledger.Block' already consumed the list-length header.
  -- We just sequence the 6 items here (no inner 'decodeListLen'),
  -- matching the Alonzo/Conway-style body decoder.
  decCBOR = do
    (bodies, bodiesAnn) <- withSlice decCBOR
    (wits, witsAnn) <- withSlice decCBOR
    let bodiesLength = length bodies
        inRange x = (0 <= x) && (x <= (bodiesLength - 1))
        witsLength = length wits
    (auxData, auxDataAnn) <- withSlice $ do
      auxDataMap <- decCBOR
      auxDataSeqDecoder bodiesLength auxDataMap

    (isValIdxs, isValAnn) <- withSlice decCBOR
    let validFlags = alignedValidFlags bodiesLength isValIdxs
    unless (bodiesLength == witsLength) $
      fail $
        "different number of transaction bodies ("
          <> show bodiesLength
          <> ") and witness sets ("
          <> show witsLength
          <> ")"
    unless (all inRange isValIdxs) $
      fail $
        "Some IsValid index is not in the range: 0 .. "
          ++ show (bodiesLength - 1)
          ++ ", "
          ++ show isValIdxs

    let txns =
          sequenceA $
            StrictSeq.forceToStrict $
              Seq.zipWith4 dijkstraSegwitTx bodies wits validFlags auxData

    -- Each cert slot is always present as 'encodeNullStrictMaybe'
    -- (CBOR null when absent, value when present).
    (leios, leiosSliceAnn) <-
      withSlice (decodeNullStrictMaybe decodeLeiosCert)
    (peras, perasSliceAnn) <-
      withSlice (decodeNullStrictMaybe (decCBOR @PerasCert))
    let mbLeiosCert = pure leios
        mbLeiosCertAnn = Just <$> leiosSliceAnn
        mbPerasCert = pure peras
        mbPerasCertAnn = Just <$> perasSliceAnn

    pure $
      DijkstraBlockBodyInternal
        <$> txns
        <*> mbLeiosCert
        <*> mbPerasCert
        <*> ( hashDijkstraSegWits
                <$> bodiesAnn
                <*> witsAnn
                <*> auxDataAnn
                <*> isValAnn
                <*> mbLeiosCertAnn
                <*> mbPerasCertAnn
            )
        <*> bodiesAnn
        <*> witsAnn
        <*> auxDataAnn
        <*> isValAnn
        <*> mbLeiosCertAnn
        <*> mbPerasCertAnn

--------------------------------------------------------------------------------
-- Internal utility functions
--------------------------------------------------------------------------------

-- | Given a sequence of transactions, return the indices of those which do not
-- validate. We store the indices of the non-validating transactions because we
-- expect this to be a much smaller set than the validating transactions.
nonValidatingIndices :: AlonzoEraTx era => StrictSeq (Tx TopTx era) -> [Int]
nonValidatingIndices (StrictSeq.fromStrict -> xs) =
  Seq.foldrWithIndex
    ( \idx tx acc ->
        if tx ^. isValidTxL == IsValid False
          then idx : acc
          else acc
    )
    []
    xs

-- | Given the number of transactions, and the set of indices for which these
-- transactions do not validate, create an aligned sequence of `IsValid`
-- flags.
--
-- This function operates much as the inverse of 'nonValidatingIndices'.
alignedValidFlags :: Int -> [Int] -> Seq.Seq IsValid
alignedValidFlags = alignedValidFlags' (-1)
  where
    alignedValidFlags' _ n [] = Seq.replicate n $ IsValid True
    alignedValidFlags' prev n (x : xs) =
      Seq.replicate (x - prev - 1) (IsValid True)
        Seq.>< IsValid False
        Seq.<| alignedValidFlags' x (n - (x - prev)) xs

-- | Construct an annotated Dijkstra style transaction.
dijkstraSegwitTx ::
  AlonzoEraTx era =>
  Annotator (TxBody TopTx era) ->
  Annotator (TxWits era) ->
  IsValid ->
  Maybe (Annotator (TxAuxData era)) ->
  Annotator (Tx TopTx era)
dijkstraSegwitTx txBodyAnn txWitsAnn txIsValid txAuxDataAnn = Annotator $ \bytes -> do
  txBody <- runAnnotator txBodyAnn bytes
  txWits <- runAnnotator txWitsAnn bytes
  txAuxData <- mapM (`runAnnotator` bytes) txAuxDataAnn
  pure $
    mkBasicTx txBody
      & witsTxL
        .~ txWits
      & auxDataTxL
        .~ maybeToStrictMaybe txAuxData
      & isValidTxL
        .~ txIsValid

-- | Lift the plain 'Leios.encodeLeiosCert' into the era-aware
-- 'Cardano.Ledger.Binary.Encoding'.
encodeLeiosCert :: LeiosCert -> Encoding
encodeLeiosCert = fromPlainEncoding . Leios.encodeLeiosCert

-- | Lift the plain 'Leios.decodeLeiosCert' into the era-aware
-- 'Cardano.Ledger.Binary.Decoder'.
decodeLeiosCert :: Decoder s LeiosCert
decodeLeiosCert = fromPlainDecoder Leios.decodeLeiosCert
