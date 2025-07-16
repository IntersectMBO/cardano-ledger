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
{-# LANGUAGE UndecidableInstances #-}
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
module Cardano.Ledger.Alonzo.BlockBody.Internal (
  AlonzoBlockBody (AlonzoBlockBody, ..),
  hashAlonzoSegWits,
  alignedValidFlags,
) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBORGroup (..),
  encCBOR,
  encodeFoldableEncoder,
  encodeFoldableMapEncoder,
  encodePreEncoded,
  encodedSizeExpr,
  serialize,
  withSlice,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.BlockBody (auxDataSeqDecoder)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, shortByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)
import Data.Proxy (Proxy (..))
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

data AlonzoBlockBody era = AlonzoBlockBodyInternal
  { abbTxs :: !(StrictSeq (Tx era))
  , abbHash :: Hash.Hash HASH EraIndependentBlockBody
  -- ^ Memoized hash to avoid recomputation. Lazy on purpose.
  , abbTxsBodyBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxBody' era)@
  , abbTxsWitsBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxWits' era)@
  , abbTxsAuxDataBytes :: BSL.ByteString
  -- ^ Bytes encoding a @'TxAuxData')@. Missing indices have
  -- 'SNothing' for metadata
  , abbTxsIsValidBytes :: BSL.ByteString
  -- ^ Bytes representing a set of integers. These are the indices of
  -- transactions with 'isValid' == False.
  }
  deriving (Generic)

instance EraBlockBody AlonzoEra where
  type BlockBody AlonzoEra = AlonzoBlockBody AlonzoEra
  mkBasicBlockBody = AlonzoBlockBody mempty
  txSeqBlockBodyL = lens abbTxs (\_ s -> AlonzoBlockBody s)
  hashBlockBody = abbHash
  numSegComponents = 4

pattern AlonzoBlockBody ::
  forall era.
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) ->
  AlonzoBlockBody era
pattern AlonzoBlockBody xs <-
  AlonzoBlockBodyInternal xs _ _ _ _ _
  where
    AlonzoBlockBody txns =
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
       in AlonzoBlockBodyInternal
            { abbTxs = txns
            , abbHash = hashAlonzoSegWits txSeqBodies txSeqWits txSeqAuxDatas txSeqIsValids
            , abbTxsBodyBytes = txSeqBodies
            , abbTxsWitsBytes = txSeqWits
            , abbTxsAuxDataBytes = txSeqAuxDatas
            , abbTxsIsValidBytes = txSeqIsValids
            }

{-# COMPLETE AlonzoBlockBody #-}

deriving via
  AllowThunksIn
    '[ "abbHash"
     , "abbTxsBodyBytes"
     , "abbTxsWitsBytes"
     , "abbTxsAuxDataBytes"
     , "abbTxsIsValidBytes"
     ]
    (AlonzoBlockBody era)
  instance
    (Typeable era, NoThunks (Tx era)) => NoThunks (AlonzoBlockBody era)

deriving stock instance Show (Tx era) => Show (AlonzoBlockBody era)

deriving stock instance Eq (Tx era) => Eq (AlonzoBlockBody era)

--------------------------------------------------------------------------------
-- Serialisation and hashing
--------------------------------------------------------------------------------

instance Era era => EncCBORGroup (AlonzoBlockBody era) where
  encCBORGroup (AlonzoBlockBodyInternal _ _ bodyBytes witsBytes metadataBytes invalidBytes) =
    encodePreEncoded $
      BSL.toStrict $
        bodyBytes <> witsBytes <> metadataBytes <> invalidBytes
  encodedGroupSizeExpr size _proxy =
    encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
  listLen _ = 4
  listLenBound _ = 4

hashAlonzoSegWits ::
  BSL.ByteString ->
  -- | Bytes for transaction bodies
  BSL.ByteString ->
  -- | Bytes for transaction witnesses
  BSL.ByteString ->
  -- | Bytes for transaction auxiliary datas
  BSL.ByteString ->
  -- | Bytes for transaction isValid flags
  Hash HASH EraIndependentBlockBody
hashAlonzoSegWits txSeqBodies txSeqWits txAuxData txSeqIsValids =
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
{-# INLINE hashAlonzoSegWits #-}

instance
  ( AlonzoEraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  DecCBOR (Annotator (AlonzoBlockBody era))
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
              Seq.zipWith4 alonzoSegwitTx bodies wits validFlags auxData
    pure $
      AlonzoBlockBodyInternal
        <$> txns
        <*> (hashAlonzoSegWits <$> bodiesAnn <*> witsAnn <*> auxDataAnn <*> isValAnn)
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
nonValidatingIndices :: AlonzoEraTx era => StrictSeq (Tx era) -> [Int]
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

-- | Construct an annotated Alonzo style transaction.
alonzoSegwitTx ::
  AlonzoEraTx era =>
  Annotator (TxBody era) ->
  Annotator (TxWits era) ->
  IsValid ->
  Maybe (Annotator (TxAuxData era)) ->
  Annotator (Tx era)
alonzoSegwitTx txBodyAnn txWitsAnn txIsValid auxDataAnn = Annotator $ \bytes ->
  let txBody = runAnnotator txBodyAnn bytes
      txWits = runAnnotator txWitsAnn bytes
      txAuxData = maybeToStrictMaybe (flip runAnnotator bytes <$> auxDataAnn)
   in mkBasicTx txBody
        & witsTxL .~ txWits
        & auxDataTxL .~ txAuxData
        & isValidTxL .~ txIsValid
