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
  DijkstraBlockBody (DijkstraBlockBody, ..),
  hashDijkstraSegWits,
  alignedValidFlags,
  mkBasicBlockBodyDijkstra,
  DijkstraEraBlockBody (..),
) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.BaseTypes (PerasCert)
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBORGroup (..),
  encCBOR,
  encodeFoldableEncoder,
  encodeFoldableMapEncoder,
  encodePreEncoded,
  serialize,
  withSlice,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Shelley.BlockBody (auxDataSeqDecoder)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, shortByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
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
  }
  deriving (Generic)

instance EraBlockBody DijkstraEra where
  type BlockBody DijkstraEra = DijkstraBlockBody DijkstraEra
  mkBasicBlockBody = mkBasicBlockBodyDijkstra
  txSeqBlockBodyL = lens dbbTxs (\bb p -> bb {dbbTxs = p})
  hashBlockBody = dbbHash
  numSegComponents = 4

mkBasicBlockBodyDijkstra ::
  ( SafeToHash (TxWits era)
  , BlockBody era ~ DijkstraBlockBody era
  , AlonzoEraTx era
  ) =>
  BlockBody era
mkBasicBlockBodyDijkstra = DijkstraBlockBody mempty SNothing
{-# INLINEABLE mkBasicBlockBodyDijkstra #-}

-- | Dijkstra-specific extensions to 'EraBlockBody'
class EraBlockBody era => DijkstraEraBlockBody era where
  perasCertBlockBodyL :: Lens' (BlockBody era) (StrictMaybe PerasCert)
  -- ^ Lens to access the optional Peras certificate in the block body

instance DijkstraEraBlockBody DijkstraEra where
  perasCertBlockBodyL = lens dbbPerasCert (\bb c -> bb {dbbPerasCert = c})

pattern DijkstraBlockBody ::
  forall era.
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx TopTx era) ->
  StrictMaybe PerasCert ->
  DijkstraBlockBody era
pattern DijkstraBlockBody xs mbPerasCert <-
  DijkstraBlockBodyInternal xs mbPerasCert _ _ _ _ _
  where
    DijkstraBlockBody txns mbPerasCert =
      let version = eraProtVerLow @era
          serializeFoldablePreEncoded x =
            serialize version $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodeIndexed <$> strictMaybeToMaybe m
            where
              encodeIndexed metadata = encCBOR index <> encodePreEncoded metadata
          txSeqBodies =
            serializeFoldablePreEncoded $ originalBytes . view bodyTxL <$> txns
          txSeqWits =
            serializeFoldablePreEncoded $ originalBytes . view witsTxL <$> txns
          txSeqAuxDatas =
            serialize version . encodeFoldableMapEncoder metaChunk $
              fmap originalBytes . view auxDataTxL <$> txns
          txSeqIsValids =
            serialize version $ encCBOR $ nonValidatingIndices txns
       in DijkstraBlockBodyInternal
            { dbbTxs = txns
            , dbbPerasCert = mbPerasCert
            , dbbHash = hashDijkstraSegWits txSeqBodies txSeqWits txSeqAuxDatas txSeqIsValids
            , dbbTxsBodyBytes = txSeqBodies
            , dbbTxsWitsBytes = txSeqWits
            , dbbTxsAuxDataBytes = txSeqAuxDatas
            , dbbTxsIsValidBytes = txSeqIsValids
            }

{-# COMPLETE DijkstraBlockBody #-}

deriving via
  AllowThunksIn
    '[ "dbbHash"
     , "dbbTxsBodyBytes"
     , "dbbTxsWitsBytes"
     , "dbbTxsAuxDataBytes"
     , "dbbTxsIsValidBytes"
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
  encCBORGroup (DijkstraBlockBodyInternal _ _ _ bodyBytes witsBytes metadataBytes invalidBytes) =
    encodePreEncoded $
      BSL.toStrict $
        bodyBytes <> witsBytes <> metadataBytes <> invalidBytes
  listLen _ = 4

hashDijkstraSegWits ::
  BSL.ByteString ->
  -- | Bytes for transaction bodies
  BSL.ByteString ->
  -- | Bytes for transaction witnesses
  BSL.ByteString ->
  -- | Bytes for transaction auxiliary datas
  BSL.ByteString ->
  -- | Bytes for transaction isValid flags
  Hash HASH EraIndependentBlockBody
hashDijkstraSegWits txSeqBodies txSeqWits txAuxData txSeqIsValids =
  coerce . hashLazy . toLazyByteString $
    hashPart txSeqBodies
      <> hashPart txSeqWits
      <> hashPart txAuxData
      <> hashPart txSeqIsValids
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

    let mbPerasCert =
          pure SNothing

    pure $
      DijkstraBlockBodyInternal
        <$> txns
        <*> mbPerasCert
        <*> (hashDijkstraSegWits <$> bodiesAnn <*> witsAnn <*> auxDataAnn <*> isValAnn)
        <*> bodiesAnn
        <*> witsAnn
        <*> auxDataAnn
        <*> isValAnn

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
