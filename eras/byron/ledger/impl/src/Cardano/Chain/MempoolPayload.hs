{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.MempoolPayload
  ( MempoolPayload,
    AMempoolPayload (..),
  )
where

import Cardano.Binary
  ( ByteSpan,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeWord8,
    encodeListLen,
    encodePreEncoded,
    enforceSize,
    recoverBytes,
  )
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.UTxO (ATxAux)
import qualified Cardano.Chain.Update as Update
import Cardano.Prelude

-- | A payload which can be submitted into or between mempools via the
-- transaction submission protocol.
type MempoolPayload = AMempoolPayload ()

-- | A payload which can be submitted into or between mempools via the
-- transaction submission protocol.
data AMempoolPayload a
  = -- | A transaction payload (transaction and witness).
    MempoolTx !(ATxAux a)
  | -- | A delegation certificate payload.
    MempoolDlg !(Delegation.ACertificate a)
  | -- | An update proposal payload.
    MempoolUpdateProposal !(Update.AProposal a)
  | -- | An update vote payload.
    MempoolUpdateVote !(Update.AVote a)
  deriving (Eq, Show, Functor)

instance ToCBOR MempoolPayload where
  toCBOR (MempoolTx tp) =
    encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR tp
  toCBOR (MempoolDlg dp) =
    encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR dp
  toCBOR (MempoolUpdateProposal upp) =
    encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR upp
  toCBOR (MempoolUpdateVote upv) =
    encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR upv

instance ToCBOR (AMempoolPayload ByteString) where
  toCBOR (MempoolTx tp) =
    encodeListLen 2 <> toCBOR (0 :: Word8) <> encodePreEncoded (recoverBytes tp)
  toCBOR (MempoolDlg dp) =
    encodeListLen 2 <> toCBOR (1 :: Word8) <> encodePreEncoded (recoverBytes dp)
  toCBOR (MempoolUpdateProposal upp) =
    encodeListLen 2 <> toCBOR (2 :: Word8) <> encodePreEncoded (recoverBytes upp)
  toCBOR (MempoolUpdateVote upv) =
    encodeListLen 2 <> toCBOR (3 :: Word8) <> encodePreEncoded (recoverBytes upv)

instance FromCBOR MempoolPayload where
  fromCBOR = void <$> fromCBOR @(AMempoolPayload ByteSpan)

instance FromCBOR (AMempoolPayload ByteSpan) where
  fromCBOR = do
    enforceSize "MempoolPayload" 2
    decodeWord8 >>= \case
      0 -> MempoolTx <$> fromCBOR
      1 -> MempoolDlg <$> fromCBOR
      2 -> MempoolUpdateProposal <$> fromCBOR
      3 -> MempoolUpdateVote <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "MempoolPayload" tag
