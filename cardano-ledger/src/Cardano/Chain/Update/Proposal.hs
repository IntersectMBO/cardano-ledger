{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Chain.Update.Proposal
  (
  -- * Proposal
    Proposal(..)
  , pattern Proposal
  , UpId

  -- * Proposal Constructors
  , signProposal
  , signatureForProposal

  -- * Proposal Formatting
  , formatMaybeProposal

  -- * ProposalBody
  , ProposalBody(..)
  , pattern ProposalBody
  )
where

import Cardano.Prelude

import qualified Data.Map.Strict as M
import Data.Text.Lazy.Builder (Builder)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Decoded(..)
  , Decoder
  , FromCBOR(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , encodeListLen
  , encodePreEncoded
  , enforceSize
  , serializeEncoding'
  , withSlice'
  )
import Cardano.Chain.Common.Attributes (dropEmptyAttributes)
import Cardano.Chain.Update.InstallerHash (InstallerHash)
import Cardano.Chain.Update.ProtocolParametersUpdate (ProtocolParametersUpdate)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Chain.Update.SystemTag (SystemTag)
import Cardano.Crypto
  ( Hash
  , ProtocolMagicId
  , VerificationKey
  , SafeSigner
  , SignTag(SignUSProposal)
  , Signature
  , hash
  , safeSign
  , safeToVerification
  )


--------------------------------------------------------------------------------
-- Proposal
--------------------------------------------------------------------------------

-- | ID of software update proposal
type UpId = Hash Proposal

-- | Create an update 'Proposal' using the provided signature.
pattern Proposal
  :: ProposalBody
  -> VerificationKey
  -> Signature ProposalBody
  -> Proposal
pattern Proposal body issuer signature <- Proposal' {body, issuer, signature}
  where
    Proposal body issuer signature =
      let bytes = serializeEncoding' $
            encodeListLen 7
              <> toCBOR (protocolVersion body)
              <> toCBOR (protocolParametersUpdate body)
              <> toCBOR (softwareVersion body)
              <> toCBOR (metadata body)
              <> toCBOR (mempty :: Map Word8 LByteString)
              <> toCBOR issuer
              <> toCBOR signature
      in Proposal' body issuer signature bytes

-- | Proposal for software update
data Proposal = Proposal'
  { body               :: !ProposalBody
  , issuer             :: !VerificationKey
  -- ^ Who proposed this UP.
  , signature          :: !(Signature ProposalBody)
  , proposalSerialized :: ByteString
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData


--------------------------------------------------------------------------------
-- Proposal Constructors
--------------------------------------------------------------------------------

-- | Create an update 'Proposal', signing it with the provided safe signer.
--
signProposal :: ProtocolMagicId -> ProposalBody -> SafeSigner -> Proposal
signProposal protocolMagicId proposalBody safeSigner =
  Proposal
    proposalBody
    (safeToVerification safeSigner)
    (signatureForProposal protocolMagicId proposalBody safeSigner)

signatureForProposal
  :: ProtocolMagicId
  -> ProposalBody
  -> SafeSigner
  -> Signature ProposalBody
signatureForProposal protocolMagicId proposalBody safeSigner =
  safeSign protocolMagicId SignUSProposal safeSigner proposalBody


--
--------------------------------------------------------------------------------
-- Proposal Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Proposal where
  toCBOR = encodePreEncoded . proposalSerialized

instance FromCBORAnnotated Proposal where
  fromCBORAnnotated' = withSlice' $
    Proposal' <$ lift (enforceSize "Proposal" 7)
      <*> withSlice' (lift fromCBORProposalBody)
      <*> lift fromCBOR
      <*> lift fromCBOR
   where
    -- Prepend byte corresponding to `encodeListLen 5`, which was used during
    -- signing
    fromCBORProposalBody :: Decoder s (ByteString -> ProposalBody)
    fromCBORProposalBody = fmap (. ("\133" <>)) $
      ProposalBody'
        <$> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*  dropEmptyAttributes

instance Decoded Proposal where
  type BaseType Proposal = Proposal
  recoverBytes = proposalSerialized


--------------------------------------------------------------------------------
-- Proposal Formatting
--------------------------------------------------------------------------------

instance B.Buildable Proposal where
  build proposal = bprint
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
    (hash proposal)
    (protocolParametersUpdate body')
    (M.keys $ metadata body')
   where
    body' = body proposal

formatMaybeProposal :: Maybe Proposal -> Builder
formatMaybeProposal = maybe "no proposal" B.build


--------------------------------------------------------------------------------
-- ProposalBody
--------------------------------------------------------------------------------

pattern ProposalBody
  :: ProtocolVersion
  -> ProtocolParametersUpdate
  -> SoftwareVersion
  -> Map SystemTag InstallerHash
  -> ProposalBody
pattern ProposalBody protocolVersion protocolParametersUpdate softwareVersion metadata <-
  ProposalBody' {protocolVersion, protocolParametersUpdate, softwareVersion, metadata}
  where
    ProposalBody protocolVersion protocolParametersUpdate softwareVersion metadata =
      let bytes = serializeEncoding' $
            encodeListLen 5
              <> toCBOR protocolVersion
              <> toCBOR protocolParametersUpdate
              <> toCBOR softwareVersion
              <> toCBOR metadata
              -- Encode empty Attributes
              <> toCBOR (mempty :: Map Word8 LByteString)
      in ProposalBody' protocolVersion protocolParametersUpdate softwareVersion metadata bytes

data ProposalBody = ProposalBody'
  { protocolVersion          :: !ProtocolVersion
  , protocolParametersUpdate :: !ProtocolParametersUpdate
  , softwareVersion          :: !SoftwareVersion
  , metadata                 :: !(Map SystemTag InstallerHash)
  -- ^ InstallerHash for each system which this update affects
  , proposalBodySerialized   :: ByteString
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData


--------------------------------------------------------------------------------
-- ProposalBody Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR ProposalBody where
  toCBOR = encodePreEncoded . proposalBodySerialized

instance FromCBORAnnotated ProposalBody where
  fromCBORAnnotated' = withSlice' . lift $ do
    enforceSize "ProposalBody" 5
    ProposalBody'
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*  dropEmptyAttributes

instance Decoded ProposalBody where
  type BaseType ProposalBody = ProposalBody
  recoverBytes = proposalBodySerialized
