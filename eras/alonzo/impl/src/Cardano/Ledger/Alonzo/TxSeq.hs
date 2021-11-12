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

-- | TxSeq. This is effectively the block body, which consists of a sequence of
-- transactions with segregated witness and metadata information.
module Cardano.Ledger.Alonzo.TxSeq
  ( TxSeq (TxSeq, txSeqTxns),
    hashTxSeq,
  )
where

import Cardano.Binary
  ( Annotator,
    FromCBOR (..),
    ToCBOR,
    encodePreEncoded,
    encodedSizeExpr,
    serializeEncoding,
    toCBOR,
    withSlice,
  )
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ValidatedTx (..), segwitTx)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript)
import Cardano.Ledger.Hashes (EraIndependentBlockBody)
import Cardano.Ledger.Keys (Hash)
import Cardano.Ledger.SafeHash (SafeToHash, originalBytes)
import Cardano.Ledger.Serialization
  ( ToCBORGroup (..),
    encodeFoldableMapEncoder,
  )
import Cardano.Ledger.Shelley.BlockChain (constructMetadata)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short (fromShort)
import Data.Coders
  ( decodeList,
    decodeMap,
    decodeSeq,
    encodeFoldable,
    encodeFoldableEncoder,
  )
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (strictMaybeToMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import NoThunks.Class (AllowThunksIn (..), NoThunks)

-- =================================================

-- $TxSeq
--
-- * TxSeq
--
-- TxSeq provides an alternate way of formatting transactions in a block, in
-- order to support segregated witnessing.

data TxSeq era = TxSeq'
  { txSeqTxns :: !(StrictSeq (ValidatedTx era)),
    txSeqBodyBytes :: BSL.ByteString,
    txSeqWitsBytes :: BSL.ByteString,
    -- | Bytes representing a (Map index metadata). Missing indices have
    -- SNothing for metadata
    txSeqMetadataBytes :: BSL.ByteString,
    -- | Bytes representing a set of integers. These are the indices of
    -- transactions with isValid == False.
    txSeqIsValidBytes :: BSL.ByteString
  }
  deriving (Generic)

pattern TxSeq ::
  forall era.
  ( Era era,
    SafeToHash (TxWitness era)
  ) =>
  StrictSeq (ValidatedTx era) ->
  TxSeq era
pattern TxSeq xs <-
  TxSeq' xs _ _ _ _
  where
    TxSeq txns =
      let serializeFoldable x =
            serializeEncoding $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodePair <$> strictMaybeToMaybe m
            where
              encodePair metadata = toCBOR index <> encodePreEncoded metadata
       in TxSeq'
            { txSeqTxns = txns,
              -- bytes encoding Seq(Core.TxBody era)
              txSeqBodyBytes =
                serializeFoldable $
                  originalBytes . getField @"body" <$> txns,
              -- bytes encoding Seq(Core.Witnesses era)
              txSeqWitsBytes =
                serializeFoldable $
                  originalBytes . getField @"wits" <$> txns,
              -- bytes encoding a (Map Int (Core.AuxiliaryData))
              txSeqMetadataBytes =
                serializeEncoding . encodeFoldableMapEncoder metaChunk $
                  fmap originalBytes . getField @"auxiliaryData" <$> txns,
              -- bytes encoding a [Int] Indexes where IsValid is False.
              txSeqIsValidBytes =
                serializeEncoding $ encodeFoldable $ nonValidatingIndices txns
            }

{-# COMPLETE TxSeq #-}

deriving via
  AllowThunksIn
    '[ "txSeqBodyBytes",
       "txSeqWitsBytes",
       "txSeqMetadataBytes",
       "txSeqIsValidBytes"
     ]
    (TxSeq era)
  instance
    (Typeable era, NoThunks (ValidatedTx era)) => NoThunks (TxSeq era)

deriving stock instance
  Show (ValidatedTx era) =>
  Show (TxSeq era)

deriving stock instance
  Eq (ValidatedTx era) =>
  Eq (TxSeq era)

--------------------------------------------------------------------------------
-- Serialisation and hashing
--------------------------------------------------------------------------------

instance
  forall era.
  (Era era) =>
  ToCBORGroup (TxSeq era)
  where
  toCBORGroup (TxSeq' _ bodyBytes witsBytes metadataBytes invalidBytes) =
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

-- | Hash a given block body
hashTxSeq ::
  forall era.
  (Era era) =>
  TxSeq era ->
  Hash (Crypto era) EraIndependentBlockBody
hashTxSeq (TxSeq' _ bodies ws md vs) =
  coerce $
    hashStrict $
      fromShort $
        mconcat
          [ hashPart bodies,
            hashPart ws,
            hashPart md,
            hashPart vs
          ]
  where
    hashStrict :: ByteString -> Hash (Crypto era) ByteString
    hashStrict = Hash.hashWith id
    hashPart = Hash.hashToBytesShort . hashStrict . BSL.toStrict

instance
  ( FromCBOR (Annotator (Core.AuxiliaryData era)),
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (Core.Witnesses era)),
    ToCBOR (Core.AuxiliaryData era),
    ToCBOR (Core.Script era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.Witnesses era),
    ValidateScript era,
    Core.Script era ~ Script era,
    Era era
  ) =>
  FromCBOR (Annotator (TxSeq era))
  where
  fromCBOR = do
    (bodies, bodiesAnn) <- withSlice $ decodeSeq fromCBOR
    (ws, witsAnn) <- withSlice $ decodeSeq fromCBOR
    let b = length bodies
        inRange x = (0 <= x) && (x <= (b - 1))
        w = length ws
    (metadata, metadataAnn) <- withSlice $
      do
        m <- decodeMap fromCBOR fromCBOR
        unless
          (all inRange (Map.keysSet m))
          ( fail
              ( "Some Auxiliarydata index is not in the range: 0 .. "
                  ++ show (b - 1)
              )
          )
        pure (constructMetadata @era b m)
    (isValIdxs, isValAnn) <- withSlice $ decodeList fromCBOR
    let vs = alignedValidFlags b isValIdxs
    unless
      (b == w)
      ( fail $
          "different number of transaction bodies ("
            <> show b
            <> ") and witness sets ("
            <> show w
            <> ")"
      )
    unless
      (all inRange isValIdxs)
      ( fail
          ( "Some IsValid index is not in the range: 0 .. "
              ++ show (b - 1)
              ++ ", "
              ++ show isValIdxs
          )
      )

    let txns =
          sequenceA $
            StrictSeq.forceToStrict $
              Seq.zipWith4 segwitTx bodies ws vs metadata
    pure $
      TxSeq'
        <$> txns
        <*> bodiesAnn
        <*> witsAnn
        <*> metadataAnn
        <*> isValAnn

--------------------------------------------------------------------------------
-- Internal utility functions
--------------------------------------------------------------------------------

-- | Given a sequence of transactions, return the indices of those which do not
-- validate. We store the indices of the non-validating transactions because we
-- expect this to be a much smaller set than the validating transactions.
nonValidatingIndices :: StrictSeq (ValidatedTx era) -> [Int]
nonValidatingIndices (StrictSeq.fromStrict -> xs) =
  Seq.foldrWithIndex
    ( \idx elt acc ->
        if getField @"isValid" elt == IsValid False
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
