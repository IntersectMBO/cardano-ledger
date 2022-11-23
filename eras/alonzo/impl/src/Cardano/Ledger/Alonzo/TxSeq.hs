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

-- | TxSeq. This is effectively the block body, which consists of a sequence of
-- transactions with segregated witness and metadata information.
module Cardano.Ledger.Alonzo.TxSeq
  ( AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
    TxSeq,
    hashTxSeq,
    hashAlonzoTxSeq,
  )
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..), alonzoSegwitTx)
import Cardano.Ledger.Binary
  ( Annotator,
    FromCBOR (..),
    ToCBORGroup (..),
    encodeFoldableEncoder,
    encodeFoldableMapEncoder,
    encodePreEncoded,
    encodedSizeExpr,
    serializeEncoding,
    toCBOR,
    withSlice,
  )
import Cardano.Ledger.Core hiding (TxSeq, hashTxSeq)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (Hash)
import Cardano.Ledger.SafeHash (SafeToHash, originalBytes)
import Cardano.Ledger.Shelley.BlockChain (constructMetadata)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (shortByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (strictMaybeToMaybe)
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

-- $TxSeq
--
-- * TxSeq
--
-- TxSeq provides an alternate way of formatting transactions in a block, in
-- order to support segregated witnessing.

data AlonzoTxSeq era = TxSeq'
  { txSeqTxns :: !(StrictSeq (Tx era)),
    -- | Bytes encoding @Seq ('AlonzoTxBody' era)@
    txSeqBodyBytes :: BSL.ByteString,
    -- | Bytes encoding @Seq ('TxWitness' era)@
    txSeqWitsBytes :: BSL.ByteString,
    -- | Bytes encoding a @Map Int ('AuxiliaryData')@. Missing indices have
    -- 'SNothing' for metadata
    txSeqMetadataBytes :: BSL.ByteString,
    -- | Bytes representing a set of integers. These are the indices of
    -- transactions with 'isValid' == False.
    txSeqIsValidBytes :: BSL.ByteString
  }
  deriving (Generic)

instance CC.Crypto c => EraSegWits (AlonzoEra c) where
  type TxSeq (AlonzoEra c) = AlonzoTxSeq (AlonzoEra c)
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4

pattern AlonzoTxSeq ::
  forall era.
  ( AlonzoEraTx era,
    SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) ->
  AlonzoTxSeq era
pattern AlonzoTxSeq xs <-
  TxSeq' xs _ _ _ _
  where
    AlonzoTxSeq txns =
      let version = eraProtVerLow @era
          serializeFoldablePreEncoded x =
            serializeEncoding version $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodeIndexed <$> strictMaybeToMaybe m
            where
              encodeIndexed metadata = toCBOR index <> encodePreEncoded metadata
       in TxSeq'
            { txSeqTxns = txns,
              txSeqBodyBytes =
                serializeFoldablePreEncoded $ originalBytes . view bodyTxL <$> txns,
              txSeqWitsBytes =
                serializeFoldablePreEncoded $ originalBytes . view witsTxL <$> txns,
              txSeqMetadataBytes =
                serializeEncoding version . encodeFoldableMapEncoder metaChunk $
                  fmap originalBytes . view auxDataTxL <$> txns,
              txSeqIsValidBytes =
                serializeEncoding version $ toCBOR $ nonValidatingIndices txns
            }

{-# COMPLETE AlonzoTxSeq #-}

type TxSeq era = AlonzoTxSeq era

{-# DEPRECATED TxSeq "Use `AlonzoTxSeq` instead" #-}

deriving via
  AllowThunksIn
    '[ "txSeqBodyBytes",
       "txSeqWitsBytes",
       "txSeqMetadataBytes",
       "txSeqIsValidBytes"
     ]
    (TxSeq era)
  instance
    (Typeable era, NoThunks (Tx era)) => NoThunks (TxSeq era)

deriving stock instance Show (Tx era) => Show (TxSeq era)

deriving stock instance Eq (Tx era) => Eq (TxSeq era)

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

hashTxSeq ::
  forall era.
  (Era era) =>
  AlonzoTxSeq era ->
  Hash (EraCrypto era) EraIndependentBlockBody
hashTxSeq = hashAlonzoTxSeq
{-# DEPRECATED hashTxSeq "Use `hashAlonzoTxSeq` instead" #-}

-- | Hash a given block body
hashAlonzoTxSeq ::
  forall era.
  (Era era) =>
  AlonzoTxSeq era ->
  Hash (EraCrypto era) EraIndependentBlockBody
hashAlonzoTxSeq (TxSeq' _ bodies ws md vs) =
  coerce $
    hashStrict $
      BSL.toStrict $
        toLazyByteString $
          mconcat
            [ hashPart bodies,
              hashPart ws,
              hashPart md,
              hashPart vs
            ]
  where
    hashStrict :: ByteString -> Hash (EraCrypto era) ByteString
    hashStrict = Hash.hashWith id
    hashPart = shortByteString . Hash.hashToBytesShort . hashStrict . BSL.toStrict

instance AlonzoEraTx era => FromCBOR (Annotator (TxSeq era)) where
  fromCBOR = do
    (bodies, bodiesAnn) <- withSlice fromCBOR
    (ws, witsAnn) <- withSlice fromCBOR
    let b = length bodies
        inRange x = (0 <= x) && (x <= (b - 1))
        w = length ws
    (auxData, auxDataAnn) <- withSlice $
      do
        m <- fromCBOR
        unless
          (all inRange (Map.keysSet m))
          ( fail
              ( "Some Auxiliarydata index is not in the range: 0 .. "
                  ++ show (b - 1)
              )
          )
        pure (constructMetadata b m)
    (isValIdxs, isValAnn) <- withSlice fromCBOR
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
              Seq.zipWith4 alonzoSegwitTx bodies ws vs auxData
    pure $
      TxSeq'
        <$> txns
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
