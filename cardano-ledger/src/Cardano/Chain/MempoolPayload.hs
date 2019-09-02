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
  = MempoolTxPayload !(ATxAux a)
  -- ^ A transaction payload (transaction and witness).
  | MempoolDlgPayload !(Delegation.ACertificate a)
  -- ^ A delegation certificate payload.
  | MempoolUpdateProposalPayload !(Update.AProposal a)
  -- ^ An update proposal payload.
  | MempoolUpdateVotePayload !(Update.AVote a)
  -- ^ An update vote payload.
  deriving (Eq, Show, Functor)

instance ToCBOR MempoolPayload where
  toCBOR (MempoolTxPayload tp) =
    encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR tp
  toCBOR (MempoolDlgPayload dp) =
    encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR dp
  toCBOR (MempoolUpdateProposalPayload upp) =
    encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR upp
  toCBOR (MempoolUpdateVotePayload upv) =
    encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR upv

instance FromCBOR MempoolPayload where
  fromCBOR = void <$> fromCBOR @(AMempoolPayload ByteSpan)

instance FromCBOR (AMempoolPayload ByteSpan) where
  fromCBOR = do
    enforceSize "MempoolPayload" 2
    decodeWord8 >>= \case
      0   -> MempoolTxPayload             <$> fromCBOR
      1   -> MempoolDlgPayload            <$> fromCBOR
      2   -> MempoolUpdateProposalPayload <$> fromCBOR
      3   -> MempoolUpdateVotePayload     <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "MempoolPayload" tag
