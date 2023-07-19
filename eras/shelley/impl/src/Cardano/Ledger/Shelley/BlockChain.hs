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
  constructMetadata,
  txSeqTxns,
  bbHash,
  bBodySize,
  slotToNonce,
  --
  incrBlocks,
  coreAuxDataBytes,
  txSeqDecoder,
)
where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Nonce (..),
  StrictMaybe (..),
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
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..))
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Tx (ShelleyTx, segwitTx)
import Cardano.Ledger.Slot (SlotNo (..))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

instance Crypto c => EraSegWits (ShelleyEra c) where
  type TxSeq (ShelleyEra c) = ShelleyTxSeq (ShelleyEra c)
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
            , -- bytes encoding Seq(TxBody era)
              txSeqBodyBytes = serializeFoldable $ coreBodyBytes @era <$> txns
            , -- bytes encoding Seq(TxWits era)
              txSeqWitsBytes = serializeFoldable $ coreWitnessBytes @era <$> txns
            , -- bytes encoding a (Map Int (TxAuxData))
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
bbHash ::
  forall era.
  Era era =>
  ShelleyTxSeq era ->
  Hash (EraCrypto era) EraIndependentBlockBody
bbHash (TxSeq' _ bodies wits md) =
  coerce $
    hashStrict
      ( hashPart bodies
          <> hashPart wits
          <> hashPart md
      )
  where
    hashStrict :: ByteString -> Hash (EraCrypto era) ByteString
    hashStrict = Hash.hashWith id
    hashPart = Hash.hashToBytes . hashStrict . BSL.toStrict

-- | Given a size and a mapping from indices to maybe metadata,
--  return a sequence whose size is the size paramater and
--  whose non-Nothing values correspond to the values in the mapping.
constructMetadata ::
  forall era.
  Int ->
  Map Int (Annotator (TxAuxData era)) ->
  Seq (Maybe (Annotator (TxAuxData era)))
constructMetadata n md = fmap (`Map.lookup` md) (Seq.fromList [0 .. n - 1])

-- | The parts of the Tx in Blocks that have to have DecCBOR(Annotator x) instances.
--   These are exactly the parts that are SafeToHash.
-- | Decode a TxSeq, used in decoding a Block.
txSeqDecoder ::
  forall era.
  EraTx era =>
  Bool ->
  forall s.
  Decoder s (Annotator (ShelleyTxSeq era))
txSeqDecoder lax = do
  (bodies, bodiesAnn) <- withSlice decCBOR
  (wits, witsAnn) <- withSlice decCBOR
  let b = length bodies
      inRange x = (0 <= x) && (x <= (b - 1))
      w = length wits
  (metadata, metadataAnn) <- withSlice $
    do
      m <- decCBOR
      unless -- TODO this PR introduces this new test, That didn't used to run in the Shelley
        (lax || all inRange (Map.keysSet m)) -- Era,  Is it possible there might be some blocks, that should have been caught on the chain?
        (fail ("Some Auxiliarydata index is not in the range: 0 .. " ++ show (b - 1)))
      pure (constructMetadata @era b m)

  unless
    (lax || b == w)
    ( fail $
        "different number of transaction bodies ("
          <> show b
          <> ") and witness sets ("
          <> show w
          <> ")"
    )

  let txns =
        sequenceA $
          StrictSeq.forceToStrict $
            Seq.zipWith3 segwitTx bodies wits metadata
  pure $ TxSeq' <$> txns <*> bodiesAnn <*> witsAnn <*> metadataAnn

instance EraTx era => DecCBOR (Annotator (ShelleyTxSeq era)) where
  decCBOR = txSeqDecoder False

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonceFromNumber s

incrBlocks ::
  Bool ->
  KeyHash 'StakePool c ->
  BlocksMade c ->
  BlocksMade c
incrBlocks isOverlay hk b'@(BlocksMade b)
  | isOverlay = b'
  | otherwise = BlocksMade $ case hkVal of
      Nothing -> Map.insert hk 1 b
      Just n -> Map.insert hk (n + 1) b
  where
    hkVal = Map.lookup hk b
