{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
module Cardano.Ledger.Shelley.BlockBody.Internal (
  ShelleyBlockBody (ShelleyBlockBody, ..),
  mkBasicBlockBodyShelley,
  txSeqBlockBodyShelleyL,
  auxDataSeqDecoder,
  hashShelleySegWits,
  bBodySize,
  slotToNonce,
  --
  incrBlocks,
  coreAuxDataBytes,
) where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Nonce (..),
  StrictMaybe (..),
  maybeToStrictMaybe,
  mkNonceFromNumber,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  Decoder,
  EncCBOR (..),
  EncCBORGroup (..),
  encodeFoldableEncoder,
  encodeFoldableMapEncoder,
  encodePreEncoded,
  serialize,
  withSlice,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Tx ()
import Cardano.Ledger.Slot (SlotNo (..))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, shortByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import Data.Monoid (All (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro hiding (ix)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

data ShelleyBlockBody era = ShelleyBlockBodyInternal
  { sbbTxs :: !(StrictSeq (Tx TopTx era))
  , sbbHash :: Hash.Hash HASH EraIndependentBlockBody
  -- ^ Memoized hash to avoid recomputation. Lazy on purpose.
  , sbbTxsBodyBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxBody' era)@
  , sbbTxsWitsBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxWits' era)@
  , sbbTxsAuxDataBytes :: BSL.ByteString
  -- ^ Bytes encoding a @Seq ('TxAuxData' era)@. Missing indices have
  -- 'SNothing' for metadata
  }
  deriving (Generic)

instance EraBlockBody ShelleyEra where
  type BlockBody ShelleyEra = ShelleyBlockBody ShelleyEra
  mkBasicBlockBody = mkBasicBlockBodyShelley
  txSeqBlockBodyL = txSeqBlockBodyShelleyL
  hashBlockBody = sbbHash
  numSegComponents = 3

mkBasicBlockBodyShelley ::
  ( EraBlockBody era
  , SafeToHash (TxWits era)
  , BlockBody era ~ ShelleyBlockBody era
  ) =>
  BlockBody era
mkBasicBlockBodyShelley = ShelleyBlockBody mempty
{-# INLINEABLE mkBasicBlockBodyShelley #-}

txSeqBlockBodyShelleyL ::
  ( EraBlockBody era
  , SafeToHash (TxWits era)
  , BlockBody era ~ ShelleyBlockBody era
  ) =>
  Lens' (BlockBody era) (StrictSeq (Tx TopTx era))
txSeqBlockBodyShelleyL = lens sbbTxs (\_ s -> ShelleyBlockBody s)
{-# INLINEABLE txSeqBlockBodyShelleyL #-}

deriving via
  AllowThunksIn
    '[ "sbbHash"
     , "sbbTxsBodyBytes"
     , "sbbTxsWitsBytes"
     , "sbbTxsAuxDataBytes"
     ]
    (ShelleyBlockBody era)
  instance
    (Typeable era, NoThunks (Tx TopTx era)) => NoThunks (ShelleyBlockBody era)

deriving stock instance
  Show (Tx TopTx era) =>
  Show (ShelleyBlockBody era)

deriving stock instance
  Eq (Tx TopTx era) =>
  Eq (ShelleyBlockBody era)

-- ===========================
-- Getting bytes from pieces of a Tx

coreWitnessBytes ::
  (EraTx era, SafeToHash (TxWits era)) =>
  Tx TopTx era ->
  ByteString
coreWitnessBytes tx = originalBytes $ tx ^. witsTxL

coreBodyBytes :: EraTx era => Tx TopTx era -> ByteString
coreBodyBytes tx = originalBytes $ tx ^. bodyTxL

coreAuxDataBytes :: EraTx era => Tx TopTx era -> StrictMaybe ByteString
coreAuxDataBytes tx = originalBytes <$> tx ^. auxDataTxL

-- ===========================

-- | Constuct a BlockBody (with all it bytes) from just Tx's
pattern ShelleyBlockBody ::
  forall era.
  ( EraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx TopTx era) ->
  ShelleyBlockBody era
pattern ShelleyBlockBody xs <-
  ShelleyBlockBodyInternal xs _ _ _ _
  where
    ShelleyBlockBody txns =
      let version = eraProtVerLow @era
          serializeFoldable x =
            serialize version $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodePair <$> strictMaybeToMaybe m
            where
              encodePair metadata = encCBOR index <> encodePreEncoded metadata
          txSeqBodies = serializeFoldable $ coreBodyBytes @era <$> txns
          txSeqWits = serializeFoldable $ coreWitnessBytes @era <$> txns
          txSeqAuxDatas =
            serialize version . encodeFoldableMapEncoder metaChunk $ coreAuxDataBytes @era <$> txns
       in ShelleyBlockBodyInternal
            { sbbTxs = txns
            , sbbHash = hashShelleySegWits txSeqBodies txSeqWits txSeqAuxDatas
            , -- bytes encoding "Seq (TxBody era)"
              sbbTxsBodyBytes = txSeqBodies
            , -- bytes encoding "Seq (TxWits era)"
              sbbTxsWitsBytes = txSeqWits
            , -- bytes encoding a "Map Int TxAuxData"
              sbbTxsAuxDataBytes = txSeqAuxDatas
            }

{-# COMPLETE ShelleyBlockBody #-}

instance Era era => EncCBORGroup (ShelleyBlockBody era) where
  encCBORGroup (ShelleyBlockBodyInternal _ _ bodyBytes witsBytes metadataBytes) =
    encodePreEncoded $
      BSL.toStrict $
        bodyBytes <> witsBytes <> metadataBytes
  listLen _ = 3
  listLenBound _ = 3

hashShelleySegWits ::
  BSL.ByteString ->
  -- | Bytes for transaction bodies
  BSL.ByteString ->
  -- | Bytes for transaction witnesses
  BSL.ByteString ->
  -- | Bytes for transaction auxiliary datas
  Hash HASH EraIndependentBlockBody
hashShelleySegWits bodies wits md =
  coerce . hashLazy . toLazyByteString $
    hashPart bodies <> hashPart wits <> hashPart md
  where
    hashLazy :: BSL.ByteString -> Hash HASH ByteString
    hashLazy = Hash.hashWith id . BSL.toStrict
    {-# INLINE hashLazy #-}
    hashPart :: BSL.ByteString -> Builder
    hashPart = shortByteString . Hash.hashToBytesShort . hashLazy
    {-# INLINE hashPart #-}
{-# INLINE hashShelleySegWits #-}

auxDataSeqDecoder ::
  Int -> IntMap a -> Decoder s (Seq (Maybe a))
auxDataSeqDecoder bodiesLength auxDataMap = do
  unless
    (getAll (IntMap.foldMapWithKey (\k _ -> All (inRange k)) auxDataMap))
    (fail ("Some Auxiliarydata index is not in the range: 0 .. " ++ show (bodiesLength - 1)))
  pure (indexLookupSeq bodiesLength auxDataMap)
  where
    inRange x = (0 <= x) && (x <= (bodiesLength - 1))
    -- Given a size and a mapping from indices to maybe values,
    -- return a sequence whose size is the size parameter and
    -- whose non-Nothing values correspond to the values in the mapping.
    indexLookupSeq :: Int -> IntMap a -> Seq (Maybe a)
    indexLookupSeq n ixMap = Seq.fromList [IntMap.lookup ix ixMap | ix <- [0 .. n - 1]]

instance
  ( EraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody TopTx era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  DecCBOR (Annotator (ShelleyBlockBody era))
  where
  -- \| The parts of the Tx in Blocks that have to have DecCBOR(Annotator x) instances.
  --   These are exactly the parts that are SafeToHash.
  -- \| Decode a BlockBody, used in decoding a Block.
  decCBOR = do
    (bodies, bodiesAnn) <- withSlice decCBOR
    (wits, witsAnn) <- withSlice decCBOR
    let bodiesLength = length bodies
        witsLength = length wits
    (metadata, metadataAnn) <- withSlice $ do
      auxDataMap <- decCBOR
      auxDataSeqDecoder bodiesLength auxDataMap

    unless
      (bodiesLength == witsLength)
      ( fail $
          "different number of transaction bodies ("
            <> show bodiesLength
            <> ") and witness sets ("
            <> show witsLength
            <> ")"
      )

    let
      segWitAnnTx txBodyAnn txWitsAnn txAuxDataAnnMaybe = Annotator $ \bytes -> do
        txBody <- runAnnotator txBodyAnn bytes
        txWits <- runAnnotator txWitsAnn bytes
        txAuxData <- mapM (`runAnnotator` bytes) txAuxDataAnnMaybe
        pure $
          mkBasicTx @era txBody
            & witsTxL .~ txWits
            & auxDataTxL .~ maybeToStrictMaybe txAuxData
      txns =
        sequenceA $
          StrictSeq.forceToStrict $
            Seq.zipWith3 segWitAnnTx bodies wits metadata
      hashAnn = hashShelleySegWits <$> bodiesAnn <*> witsAnn <*> metadataAnn
    pure $ ShelleyBlockBodyInternal <$> txns <*> hashAnn <*> bodiesAnn <*> witsAnn <*> metadataAnn

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonceFromNumber s

incrBlocks ::
  Bool ->
  KeyHash StakePool ->
  BlocksMade ->
  BlocksMade
incrBlocks isOverlay hk blocksMade@(BlocksMade blocksMadeMap)
  | isOverlay = blocksMade
  | otherwise = BlocksMade $ Map.insertWith (+) hk 1 blocksMadeMap
