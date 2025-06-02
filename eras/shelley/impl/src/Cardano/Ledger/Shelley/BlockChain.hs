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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.BlockChain (
  ShelleyTxSeq (ShelleyTxSeq, txSeqTxns', TxSeq'),
  auxDataSeqDecoder,
  txSeqTxns,
  bbHash,
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
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

data ShelleyTxSeq era = TxSeq'
  { txSeqTxns' :: !(StrictSeq (Tx era))
  , txSeqHash :: Hash.Hash HASH EraIndependentBlockBody
  -- ^ Memoized hash to avoid recomputation. Lazy on purpose.
  , txSeqBodyBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxBody' era)@
  , txSeqWitsBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxWits' era)@
  , txSeqMetadataBytes :: BSL.ByteString
  -- ^ Bytes encoding a @Seq ('TxAuxData' era)@. Missing indices have
  -- 'SNothing' for metadata
  }
  deriving (Generic)

instance EraSegWits ShelleyEra where
  type TxSeq ShelleyEra = ShelleyTxSeq ShelleyEra
  fromTxSeq = txSeqTxns
  toTxSeq = ShelleyTxSeq
  hashTxSeq = txSeqHash
  numSegComponents = 3

deriving via
  AllowThunksIn
    '[ "txSeqHash"
     , "txSeqBodyBytes"
     , "txSeqWitsBytes"
     , "txSeqMetadataBytes"
     ]
    (ShelleyTxSeq era)
  instance
    (Typeable era, NoThunks (Tx era)) => NoThunks (ShelleyTxSeq era)

deriving stock instance Show (Tx era) => Show (ShelleyTxSeq era)

deriving stock instance Eq (Tx era) => Eq (ShelleyTxSeq era)

-- ===========================
-- Getting bytes from pieces of a Tx

coreWitnessBytes ::
  (EraTx era, SafeToHash (TxWits era)) =>
  Tx era ->
  ByteString
coreWitnessBytes tx = originalBytes $ tx ^. witsTxL

coreBodyBytes :: EraTx era => Tx era -> ByteString
coreBodyBytes tx = originalBytes $ tx ^. bodyTxL

coreAuxDataBytes :: EraTx era => Tx era -> StrictMaybe ByteString
coreAuxDataBytes tx = originalBytes <$> tx ^. auxDataTxL

-- ===========================

-- | Constuct a TxSeq (with all it bytes) from just Tx's
pattern ShelleyTxSeq ::
  forall era.
  ( EraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) ->
  ShelleyTxSeq era
pattern ShelleyTxSeq xs <-
  TxSeq' xs _ _ _ _
  where
    ShelleyTxSeq txns =
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
       in TxSeq'
            { txSeqTxns' = txns
            , txSeqHash = hashShelleySegWits txSeqBodies txSeqWits txSeqAuxDatas
            , -- bytes encoding "Seq (TxBody era)"
              txSeqBodyBytes = txSeqBodies
            , -- bytes encoding "Seq (TxWits era)"
              txSeqWitsBytes = txSeqWits
            , -- bytes encoding a "Map Int TxAuxData"
              txSeqMetadataBytes = txSeqAuxDatas
            }

{-# COMPLETE ShelleyTxSeq #-}

txSeqTxns :: ShelleyTxSeq era -> StrictSeq (Tx era)
txSeqTxns (TxSeq' ts _ _ _ _) = ts

instance
  forall era.
  Era era =>
  EncCBORGroup (ShelleyTxSeq era)
  where
  encCBORGroup (TxSeq' _ _ bodyBytes witsBytes metadataBytes) =
    encodePreEncoded $
      BSL.toStrict $
        bodyBytes <> witsBytes <> metadataBytes
  encodedGroupSizeExpr size _proxy =
    encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
  listLen _ = 3
  listLenBound _ = 3

-- | Hash a given block body
bbHash :: ShelleyTxSeq era -> Hash HASH EraIndependentBlockBody
bbHash = txSeqHash

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
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  DecCBOR (Annotator (ShelleyTxSeq era))
  where
  -- \| The parts of the Tx in Blocks that have to have DecCBOR(Annotator x) instances.
  --   These are exactly the parts that are SafeToHash.
  -- \| Decode a TxSeq, used in decoding a Block.
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
      segWitAnnTx bodyAnn witsAnn' metaAnn = Annotator $ \bytes ->
        let body' = runAnnotator bodyAnn bytes
            witnessSet = runAnnotator witsAnn' bytes
            metadata' = flip runAnnotator bytes <$> metaAnn
         in mkBasicTx @era body'
              & witsTxL .~ witnessSet
              & auxDataTxL .~ maybeToStrictMaybe metadata'
      txns =
        sequenceA $
          StrictSeq.forceToStrict $
            Seq.zipWith3 segWitAnnTx bodies wits metadata
      hashAnn = hashShelleySegWits <$> bodiesAnn <*> witsAnn <*> metadataAnn
    pure $ TxSeq' <$> txns <*> hashAnn <*> bodiesAnn <*> witsAnn <*> metadataAnn

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonceFromNumber s

incrBlocks ::
  Bool ->
  KeyHash 'StakePool ->
  BlocksMade ->
  BlocksMade
incrBlocks isOverlay hk b'@(BlocksMade b)
  | isOverlay = b'
  | otherwise = BlocksMade $ case hkVal of
      Nothing -> Map.insert hk 1 b
      Just n -> Map.insert hk (n + 1) b
  where
    hkVal = Map.lookup hk b
