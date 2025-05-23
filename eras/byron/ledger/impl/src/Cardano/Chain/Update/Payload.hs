{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Chain.Update.Payload (
  APayload (..),
  Payload,
  payload,
) where

import Cardano.Chain.Update.Proposal (
  AProposal,
  Proposal,
  formatMaybeProposal,
 )
import Cardano.Chain.Update.Vote (
  AVote,
  Vote,
  formatVoteShort,
 )
import Cardano.Ledger.Binary (
  Annotated (..),
  ByteSpan,
  DecCBOR (..),
  Decoded (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  annotatedDecoder,
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Formatting (bprint)
import qualified Formatting.Buildable as B

-- | Update System payload
data APayload a = APayload
  { payloadProposal :: !(Maybe (AProposal a))
  , payloadVotes :: ![AVote a]
  , payloadAnnotation :: a
  }
  deriving (Eq, Show, Generic, Functor)
  deriving anyclass (NFData)

type Payload = APayload ()

payload :: Maybe Proposal -> [Vote] -> Payload
payload p v = APayload p v ()

instance Decoded (APayload ByteString) where
  type BaseType (APayload ByteString) = Payload
  recoverBytes = payloadAnnotation

instance B.Buildable Payload where
  build p
    | null (payloadVotes p) =
        formatMaybeProposal (payloadProposal p) <> ", no votes"
    | otherwise =
        formatMaybeProposal (payloadProposal p)
          <> bprint
            ("\n    votes: " . listJson)
            (map formatVoteShort (payloadVotes p))

-- Used for debugging purposes only
instance ToJSON a => ToJSON (APayload a)

instance ToCBOR Payload where
  toCBOR = toByronCBOR

instance FromCBOR Payload where
  fromCBOR = fromByronCBOR

instance FromCBOR (APayload ByteSpan) where
  fromCBOR = fromByronCBOR

instance EncCBOR Payload where
  encCBOR p =
    encodeListLen 2 <> encCBOR (payloadProposal p) <> encCBOR (payloadVotes p)

instance DecCBOR Payload where
  decCBOR = void <$> decCBOR @(APayload ByteSpan)

instance DecCBOR (APayload ByteSpan) where
  decCBOR = do
    Annotated (proposal, votes) byteSpan <- annotatedDecoder $ do
      enforceSize "Update.Payload" 2
      (,) <$> decCBOR <*> decCBOR
    pure $ APayload proposal votes byteSpan
