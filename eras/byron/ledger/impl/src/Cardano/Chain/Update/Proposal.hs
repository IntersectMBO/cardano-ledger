{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 900
-- this is needed for 9.2: recoveryBytes = annotation
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
#endif

module Cardano.Chain.Update.Proposal (
  -- * Proposal
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

import Cardano.Chain.Common.Attributes (dropEmptyAttributes)
import Cardano.Chain.Update.InstallerHash (InstallerHash)
import Cardano.Chain.Update.ProtocolParametersUpdate (ProtocolParametersUpdate)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Chain.Update.SystemTag (SystemTag)
import Cardano.Crypto (
  Hash,
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
import Cardano.Ledger.Binary (
  Annotated (Annotated, unAnnotated),
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
  { aBody :: !(Annotated ProposalBody a)
  , issuer :: !VerificationKey
  -- ^ Who proposed this UP.
  , signature :: !(Signature ProposalBody)
  , annotation :: !a
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
  toCBOR = toByronCBOR

instance FromCBOR Proposal where
  fromCBOR = fromByronCBOR

instance EncCBOR Proposal where
  encCBOR proposal =
    encodeListLen 7
      <> encCBOR (protocolVersion body')
      <> encCBOR (protocolParametersUpdate body')
      <> encCBOR (softwareVersion body')
      <> encCBOR (metadata body')
      <> encCBOR (mempty :: Map Word8 LByteString)
      <> encCBOR (issuer proposal)
      <> encCBOR (signature proposal)
    where
      body' = body proposal

instance DecCBOR Proposal where
  decCBOR = void <$> decCBOR @(AProposal ByteSpan)

instance FromCBOR (AProposal ByteSpan) where
  fromCBOR = fromByronCBOR

instance DecCBOR (AProposal ByteSpan) where
  decCBOR = do
    Annotated (pb, vk, sig) byteSpan <- annotatedDecoder $ do
      enforceSize "Proposal" 7
      pb <-
        annotatedDecoder
          ( ProposalBody
              <$> decCBOR
              <*> decCBOR
              <*> decCBOR
              <*> decCBOR
              <* dropEmptyAttributes
          )
      vk <- decCBOR
      sig <- decCBOR
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
  { protocolVersion :: !ProtocolVersion
  , protocolParametersUpdate :: !ProtocolParametersUpdate
  , softwareVersion :: !SoftwareVersion
  , metadata :: !(Map SystemTag InstallerHash)
  -- ^ InstallerHash for each system which this update affects
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData)

-- Used for debugging purposes only
instance ToJSON ProposalBody

--------------------------------------------------------------------------------
-- ProposalBody Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR ProposalBody where
  toCBOR = toByronCBOR

instance FromCBOR ProposalBody where
  fromCBOR = fromByronCBOR

instance EncCBOR ProposalBody where
  encCBOR pb =
    encodeListLen 5
      <> encCBOR (protocolVersion pb)
      <> encCBOR (protocolParametersUpdate pb)
      <> encCBOR (softwareVersion pb)
      <> encCBOR (metadata pb)
      -- Encode empty Attributes
      <> encCBOR (mempty :: Map Word8 LByteString)

instance DecCBOR ProposalBody where
  decCBOR = do
    enforceSize "ProposalBody" 5
    ProposalBody
      <$> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <* dropEmptyAttributes

-- | Prepend byte corresponding to `encodeListLen 5`, which was used during
--   signing
recoverProposalSignedBytes ::
  Annotated ProposalBody ByteString -> Annotated ProposalBody ByteString
recoverProposalSignedBytes = fmap ("\133" <>)
