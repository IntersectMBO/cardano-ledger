{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.MempoolPayload (
  MempoolPayload,
  AMempoolPayload (..),
)
where

import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.UTxO (ATxAux)
import qualified Cardano.Chain.Update as Update
import Cardano.Ledger.Binary (
  ByteSpan,
  DecCBOR (..),
  DecoderError (..),
  EncCBOR (..),
  cborError,
  decodeWord8,
  encodeListLen,
  encodePreEncoded,
  enforceSize,
  recoverBytes,
 )
import Cardano.Prelude hiding (cborError)

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

instance EncCBOR MempoolPayload where
  encCBOR (MempoolTx tp) =
    encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR tp
  encCBOR (MempoolDlg dp) =
    encodeListLen 2 <> encCBOR (1 :: Word8) <> encCBOR dp
  encCBOR (MempoolUpdateProposal upp) =
    encodeListLen 2 <> encCBOR (2 :: Word8) <> encCBOR upp
  encCBOR (MempoolUpdateVote upv) =
    encodeListLen 2 <> encCBOR (3 :: Word8) <> encCBOR upv

instance EncCBOR (AMempoolPayload ByteString) where
  encCBOR (MempoolTx tp) =
    encodeListLen 2 <> encCBOR (0 :: Word8) <> encodePreEncoded (recoverBytes tp)
  encCBOR (MempoolDlg dp) =
    encodeListLen 2 <> encCBOR (1 :: Word8) <> encodePreEncoded (recoverBytes dp)
  encCBOR (MempoolUpdateProposal upp) =
    encodeListLen 2 <> encCBOR (2 :: Word8) <> encodePreEncoded (recoverBytes upp)
  encCBOR (MempoolUpdateVote upv) =
    encodeListLen 2 <> encCBOR (3 :: Word8) <> encodePreEncoded (recoverBytes upv)

instance DecCBOR MempoolPayload where
  decCBOR = void <$> decCBOR @(AMempoolPayload ByteSpan)

instance DecCBOR (AMempoolPayload ByteSpan) where
  decCBOR = do
    enforceSize "MempoolPayload" 2
    decodeWord8 >>= \case
      0 -> MempoolTx <$> decCBOR
      1 -> MempoolDlg <$> decCBOR
      2 -> MempoolUpdateProposal <$> decCBOR
      3 -> MempoolUpdateVote <$> decCBOR
      tag -> cborError $ DecoderErrorUnknownTag "MempoolPayload" tag
