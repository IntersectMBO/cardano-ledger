{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.MempoolPayload
  ( MempoolPayload
  , AMempoolPayload (..)
  )
where

import Cardano.Prelude

import Cardano.Binary
  ( ByteSpan
  , DecoderError(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , decodeWord8
  , encodeListLen
  , enforceSize
  )
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.UTxO (ATxAux)
import qualified Cardano.Chain.Update as Update

-- | A payload which can be submitted into or between mempools via the
-- transaction submission protocol.
type MempoolPayload = AMempoolPayload ()

-- | A payload which can be submitted into or between mempools via the
-- transaction submission protocol.
data AMempoolPayload a
  = MempoolTx !(ATxAux a)
  -- ^ A transaction payload (transaction and witness).
  | MempoolDlg !(Delegation.ACertificate a)
  -- ^ A delegation certificate payload.
  | MempoolUpdateProposal !(Update.AProposal a)
  -- ^ An update proposal payload.
  | MempoolUpdateVote !(Update.AVote a)
  -- ^ An update vote payload.
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

instance FromCBOR MempoolPayload where
  fromCBOR = void <$> fromCBOR @(AMempoolPayload ByteSpan)

instance FromCBOR (AMempoolPayload ByteSpan) where
  fromCBOR = do
    enforceSize "MempoolPayload" 2
    decodeWord8 >>= \case
      0   -> MempoolTx             <$> fromCBOR
      1   -> MempoolDlg            <$> fromCBOR
      2   -> MempoolUpdateProposal <$> fromCBOR
      3   -> MempoolUpdateVote     <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "MempoolPayload" tag
