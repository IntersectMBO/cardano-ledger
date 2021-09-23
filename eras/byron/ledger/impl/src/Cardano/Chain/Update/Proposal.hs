{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Chain.Update.Proposal
  ( -- * Proposal
    AProposal (..),
    Proposal,
    UpId,

    -- * Proposal Constructors
    unsafeProposal,
    signProposal,
    signatureForProposal,

    -- * Proposal Accessors
    body,
    recoverUpId,

    -- * Proposal Formatting
    formatMaybeProposal,

    -- * ProposalBody
    ProposalBody (..),

    -- * ProposalBody Binary Serialization
    recoverProposalSignedBytes,
  )
where

import Cardano.Binary
  ( Annotated (..),
    ByteSpan,
    Decoded (..),
    FromCBOR (..),
    ToCBOR (..),
    annotatedDecoder,
    encodeListLen,
    enforceSize,
  )
import Cardano.Chain.Common.Attributes (dropEmptyAttributes)
import Cardano.Chain.Update.InstallerHash (InstallerHash)
import Cardano.Chain.Update.ProtocolParametersUpdate (ProtocolParametersUpdate)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Chain.Update.SystemTag (SystemTag)
import Cardano.Crypto
  ( Hash,
    ProtocolMagicId,
    SafeSigner,
    SignTag (SignUSProposal),
    Signature,
    VerificationKey,
    hashDecoded,
    safeSign,
    safeToVerification,
    serializeCborHash,
  )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as M
import Data.Text.Lazy.Builder (Builder)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B

--------------------------------------------------------------------------------
-- Proposal
--------------------------------------------------------------------------------

-- | ID of software update proposal
type UpId = Hash Proposal

-- | Proposal for software update
data AProposal a = AProposal
  { aBody :: !(Annotated ProposalBody a),
    -- | Who proposed this UP.
    issuer :: !VerificationKey,
    signature :: !(Signature ProposalBody),
    annotation :: !a
  }
  deriving (Eq, Show, Generic, Functor)
  deriving anyclass (NFData)

type Proposal = AProposal ()

-- Used for debugging purposes only
instance ToJSON a => ToJSON (AProposal a)

--------------------------------------------------------------------------------
-- Proposal Constructors
--------------------------------------------------------------------------------

-- | Create an update 'Proposal', signing it with the provided safe signer.
signProposal :: ProtocolMagicId -> ProposalBody -> SafeSigner -> Proposal
signProposal protocolMagicId proposalBody safeSigner =
  unsafeProposal
    proposalBody
    (safeToVerification safeSigner)
    (signatureForProposal protocolMagicId proposalBody safeSigner)

signatureForProposal ::
  ProtocolMagicId ->
  ProposalBody ->
  SafeSigner ->
  Signature ProposalBody
signatureForProposal protocolMagicId proposalBody safeSigner =
  safeSign protocolMagicId SignUSProposal safeSigner proposalBody

-- | Create an update 'Proposal' using the provided signature.
unsafeProposal :: ProposalBody -> VerificationKey -> Signature ProposalBody -> Proposal
unsafeProposal b k s = AProposal (Annotated b ()) k s ()

--------------------------------------------------------------------------------
-- Proposal Accessors
--------------------------------------------------------------------------------

body :: AProposal a -> ProposalBody
body = unAnnotated . aBody

recoverUpId :: AProposal ByteString -> UpId
recoverUpId = hashDecoded

--------------------------------------------------------------------------------
-- Proposal Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Proposal where
  toCBOR proposal =
    encodeListLen 7
      <> toCBOR (protocolVersion body')
      <> toCBOR (protocolParametersUpdate body')
      <> toCBOR (softwareVersion body')
      <> toCBOR (metadata body')
      <> toCBOR (mempty :: Map Word8 LByteString)
      <> toCBOR (issuer proposal)
      <> toCBOR (signature proposal)
    where
      body' = body proposal

instance FromCBOR Proposal where
  fromCBOR = void <$> fromCBOR @(AProposal ByteSpan)

instance FromCBOR (AProposal ByteSpan) where
  fromCBOR = do
    Annotated (pb, vk, sig) byteSpan <- annotatedDecoder $ do
      enforceSize "Proposal" 7
      pb <-
        annotatedDecoder
          ( ProposalBody
              <$> fromCBOR
              <*> fromCBOR
              <*> fromCBOR
              <*> fromCBOR
              <* dropEmptyAttributes
          )
      vk <- fromCBOR
      sig <- fromCBOR
      pure (pb, vk, sig)
    pure $ AProposal pb vk sig byteSpan

instance Decoded (AProposal ByteString) where
  type BaseType (AProposal ByteString) = Proposal
  recoverBytes = annotation

--------------------------------------------------------------------------------
-- Proposal Formatting
--------------------------------------------------------------------------------

instance B.Buildable (AProposal ()) where
  build proposal =
    bprint
      ( build
          . " { block v"
          . build
          . ", UpId: "
          . build
          . ", "
          . build
          . ", tags: "
          . listJson
          . " }"
      )
      (softwareVersion body')
      (protocolVersion body')
      (serializeCborHash proposal)
      (protocolParametersUpdate body')
      (M.keys $ metadata body')
    where
      body' = body proposal

formatMaybeProposal :: Maybe Proposal -> Builder
formatMaybeProposal = maybe "no proposal" B.build

--------------------------------------------------------------------------------
-- ProposalBody
--------------------------------------------------------------------------------

data ProposalBody = ProposalBody
  { protocolVersion :: !ProtocolVersion,
    protocolParametersUpdate :: !ProtocolParametersUpdate,
    softwareVersion :: !SoftwareVersion,
    -- | InstallerHash for each system which this update affects
    metadata :: !(Map SystemTag InstallerHash)
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData)

-- Used for debugging purposes only
instance ToJSON ProposalBody

--------------------------------------------------------------------------------
-- ProposalBody Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR ProposalBody where
  toCBOR pb =
    encodeListLen 5
      <> toCBOR (protocolVersion pb)
      <> toCBOR (protocolParametersUpdate pb)
      <> toCBOR (softwareVersion pb)
      <> toCBOR (metadata pb)
      -- Encode empty Attributes
      <> toCBOR (mempty :: Map Word8 LByteString)

instance FromCBOR ProposalBody where
  fromCBOR = do
    enforceSize "ProposalBody" 5
    ProposalBody
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <* dropEmptyAttributes

-- | Prepend byte corresponding to `encodeListLen 5`, which was used during
--   signing
recoverProposalSignedBytes ::
  Annotated ProposalBody ByteString -> Annotated ProposalBody ByteString
recoverProposalSignedBytes = fmap ("\133" <>)
