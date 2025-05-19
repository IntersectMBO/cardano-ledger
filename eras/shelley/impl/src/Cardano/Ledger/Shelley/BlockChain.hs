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

module Cardano.Ledger.Shelley.BlockChain (
  ShelleyTxSeq (ShelleyTxSeq, txSeqTxns', TxSeq'),
  auxDataSeqDecoder,
  txSeqTxns,
  bbHash,
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
  mkNonceFromNumber,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (
  Annotated (..),
  Annotator,
  DecCBOR (decCBOR),
  Decoder,
  EncCBOR (..),
  EncCBORGroup (..),
  decodeAnnotated,
  encodeFoldableEncoder,
  encodeFoldableMapEncoder,
  encodePreEncoded,
  serialize,
  withSlice,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Tx (ShelleyTx, segWitAnnTx, segWitTx)
import Cardano.Ledger.Slot (SlotNo (..))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
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
import Lens.Micro ((^.))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

data ShelleyTxSeq era = TxSeq'
  { txSeqTxns' :: !(StrictSeq (ShelleyTx era))
  , txSeqBodyBytes :: BSL.ByteString
  , txSeqWitsBytes :: BSL.ByteString
  , txSeqMetadataBytes :: BSL.ByteString
  -- bytes representing a (Map index metadata). Missing indices have SNothing for metadata
  }
  deriving (Generic)

instance EraSegWits ShelleyEra where
  type TxSeq ShelleyEra = ShelleyTxSeq ShelleyEra
  fromTxSeq = txSeqTxns
  toTxSeq = ShelleyTxSeq
  hashTxSeq = bbHash
  numSegComponents = 3

deriving via
  AllowThunksIn
    '[ "txSeqBodyBytes"
     , "txSeqWitsBytes"
     , "txSeqMetadataBytes"
     ]
    (ShelleyTxSeq era)
  instance
    (Typeable era, NoThunks (ShelleyTx era)) => NoThunks (ShelleyTxSeq era)

deriving stock instance
  Show (ShelleyTx era) =>
  Show (ShelleyTxSeq era)

deriving stock instance
  Eq (ShelleyTx era) =>
  Eq (ShelleyTxSeq era)

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
  , Tx era ~ ShelleyTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) ->
  ShelleyTxSeq era
pattern ShelleyTxSeq xs <-
  TxSeq' xs _ _ _
  where
    ShelleyTxSeq txns =
      let version = eraProtVerLow @era
          serializeFoldable x =
            serialize version $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodePair <$> strictMaybeToMaybe m
            where
              encodePair metadata = encCBOR index <> encodePreEncoded metadata
       in TxSeq'
            { txSeqTxns' = txns
            , -- bytes encoding "Seq (TxBody era)"
              txSeqBodyBytes = serializeFoldable $ coreBodyBytes @era <$> txns
            , -- bytes encoding "Seq (TxWits era)"
              txSeqWitsBytes = serializeFoldable $ coreWitnessBytes @era <$> txns
            , -- bytes encoding a "Map Int TxAuxData"
              txSeqMetadataBytes =
                serialize version . encodeFoldableMapEncoder metaChunk $
                  coreAuxDataBytes @era <$> txns
            }

{-# COMPLETE ShelleyTxSeq #-}

txSeqTxns :: ShelleyTxSeq era -> StrictSeq (ShelleyTx era)
txSeqTxns (TxSeq' ts _ _ _) = ts

instance
  forall era.
  Era era =>
  EncCBORGroup (ShelleyTxSeq era)
  where
  encCBORGroup (TxSeq' _ bodyBytes witsBytes metadataBytes) =
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
bbHash (TxSeq' _ bodies wits md) =
  coerce $
    hashStrict
      ( hashPart bodies
          <> hashPart wits
          <> hashPart md
      )
  where
    hashStrict :: ByteString -> Hash HASH ByteString
    hashStrict = Hash.hashWith id
    hashPart = Hash.hashToBytes . hashStrict . BSL.toStrict

auxDataSeqDecoder ::
  Int -> IntMap a -> Bool -> Decoder s (Seq (Maybe a))
auxDataSeqDecoder bodiesLength auxDataMap lax = do
  unless
    (lax || getAll (IntMap.foldMapWithKey (\k _ -> All (inRange k)) auxDataMap))
    (fail ("Some Auxiliarydata index is not in the range: 0 .. " ++ show (bodiesLength - 1)))
  pure (indexLookupSeq bodiesLength auxDataMap)
  where
    inRange x = (0 <= x) && (x <= (bodiesLength - 1))
    -- Given a size and a mapping from indices to maybe values,
    -- return a sequence whose size is the size parameter and
    -- whose non-Nothing values correspond to the values in the mapping.
    indexLookupSeq :: Int -> IntMap a -> Seq (Maybe a)
    indexLookupSeq n ixMap = Seq.fromList [IntMap.lookup ix ixMap | ix <- [0 .. n - 1]]

instance EraTx era => DecCBOR (ShelleyTxSeq era) where
  decCBOR = do
    Annotated bodies bodiesBytes <- decodeAnnotated decCBOR
    Annotated wits witsBytes <- decodeAnnotated decCBOR
    Annotated (auxDataMap :: IntMap (TxAuxData era)) auxDataBytes <- decodeAnnotated decCBOR
    let bodiesLength = length bodies
    auxData <-
      fmap (fmap runIdentity)
        <$> auxDataSeqDecoder bodiesLength (fmap pure auxDataMap) False

    let witsLength = length wits
    unless
      (bodiesLength == witsLength)
      ( fail $
          "different number of transaction bodies ("
            <> show bodiesLength
            <> ") and witness sets ("
            <> show witsLength
            <> ")"
      )
    let txs =
          StrictSeq.forceToStrict $
            Seq.zipWith3 segWitTx bodies wits auxData
    pure $ TxSeq' txs bodiesBytes witsBytes auxDataBytes

instance
  ( EraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  DecCBOR (Annotator (ShelleyTxSeq era))
  where
  decCBOR = txSeqDecoder False

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

-- | The parts of the Tx in Blocks that have to have DecCBOR(Annotator x) instances.
--   These are exactly the parts that are SafeToHash.
-- | Decode a TxSeq, used in decoding a Block.
txSeqDecoder ::
  ( EraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  Bool ->
  Decoder s (Annotator (ShelleyTxSeq era))
txSeqDecoder lax = do
  (bodies, bodiesAnn) <- withSlice decCBOR
  (wits, witsAnn) <- withSlice decCBOR
  let bodiesLength = length bodies
      witsLength = length wits
  (metadata, metadataAnn) <- withSlice $ do
    auxDataMap <- decCBOR
    auxDataSeqDecoder bodiesLength auxDataMap lax

  unless
    (lax || bodiesLength == witsLength)
    ( fail $
        "different number of transaction bodies ("
          <> show bodiesLength
          <> ") and witness sets ("
          <> show witsLength
          <> ")"
    )

  let txns =
        sequenceA $
          StrictSeq.forceToStrict $
            Seq.zipWith3 segWitAnnTx bodies wits metadata
  pure $ TxSeq' <$> txns <*> bodiesAnn <*> witsAnn <*> metadataAnn
