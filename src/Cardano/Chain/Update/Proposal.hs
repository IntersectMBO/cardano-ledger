{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Chain.Update.Proposal
  (
  -- * Proposal
    AProposal(..)
  , Proposal
  , UpId

  -- * Proposal Constructors
  , mkProposal
  , signProposal

  -- * Proposal Accessors
  , body
  , recoverUpId

  -- * Proposal Formatting
  , formatMaybeProposal

  -- * ProposalBody
  , ProposalBody(..)

  -- * ProposalBody Binary Serialization
  , recoverProposalSignedBytes
  )
where

import Cardano.Prelude

import qualified Data.Map.Strict as M
import Data.Text.Lazy.Builder (Builder)
import Formatting (bprint, build, builder)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan
  , Decoded(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , annotatedDecoder
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Common.Attributes (Attributes, attributesAreKnown)
import Cardano.Chain.Update.Data (UpdateData)
import Cardano.Chain.Update.ProtocolParametersUpdate (ProtocolParametersUpdate)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Chain.Update.SystemTag (SystemTag)
import Cardano.Crypto
  ( Hash
  , ProtocolMagicId
  , PublicKey
  , SafeSigner
  , SignTag(SignUSProposal)
  , Signature
  , hash
  , hashDecoded
  , safeSign
  , safeToPublic
  )


--------------------------------------------------------------------------------
-- Proposal
--------------------------------------------------------------------------------

-- | ID of software update proposal
type UpId = Hash Proposal

-- | Proposal for software update
data AProposal a = AProposal
  { aBody      :: !(Annotated ProposalBody a)
  , issuer     :: !PublicKey
  -- ^ Who proposed this UP.
  , signature  :: !(Signature ProposalBody)
  , annotation :: !a
  } deriving (Eq, Show, Generic, Functor)
    deriving anyclass NFData

type Proposal = AProposal ()


--------------------------------------------------------------------------------
-- Proposal Constructors
--------------------------------------------------------------------------------

mkProposal :: ProposalBody -> PublicKey -> Signature ProposalBody -> Proposal
mkProposal b k s = AProposal (Annotated b ()) k s ()

signProposal :: ProtocolMagicId -> ProposalBody -> SafeSigner -> Proposal
signProposal pm pb ss = mkProposal pb (safeToPublic ss) sig
  where sig = safeSign pm SignUSProposal ss pb


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
      <> toCBOR (attributes body')
      <> toCBOR (issuer proposal)
      <> toCBOR (signature proposal)
    where body' = body proposal

instance FromCBOR Proposal where
  fromCBOR = void <$> fromCBOR @(AProposal ByteSpan)

instance FromCBOR (AProposal ByteSpan) where
  fromCBOR = do
    Annotated (pb, pk, sig) byteSpan <- annotatedDecoder $ do
      enforceSize "Proposal" 7
      pb <- annotatedDecoder
        (   ProposalBody
        <$> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        )
      pk  <- fromCBOR
      sig <- fromCBOR
      pure (pb, pk, sig)
    pure $ AProposal pb pk sig byteSpan

instance Decoded (AProposal ByteString) where
  type BaseType (AProposal ByteString) = Proposal
  recoverBytes = annotation


--------------------------------------------------------------------------------
-- Proposal Formatting
--------------------------------------------------------------------------------

instance B.Buildable (AProposal ()) where
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
    . ", "
    . builder
    . " }"
    )
    (softwareVersion body')
    (protocolVersion body')
    (hash proposal)
    (protocolParametersUpdate body')
    (M.keys $ metadata body')
    attrsBuilder
   where
    body' = body proposal
    attrs = attributes body'
    attrsBuilder
      | attributesAreKnown attrs = "no attributes"
      | otherwise                = bprint ("attributes: " . build) attrs

formatMaybeProposal :: Maybe Proposal -> Builder
formatMaybeProposal = maybe "no proposal" B.build


--------------------------------------------------------------------------------
-- ProposalBody
--------------------------------------------------------------------------------

data ProposalBody = ProposalBody
  { protocolVersion          :: !ProtocolVersion
  , protocolParametersUpdate :: !ProtocolParametersUpdate
  , softwareVersion          :: !SoftwareVersion
  , metadata                 :: !(Map SystemTag UpdateData)
  -- ^ UpdateData for each system which this update affects. It must be
  --   non-empty.
  , attributes               :: !(Attributes ())
  -- ^ Attributes which are currently empty, but provide extensibility
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData


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
      <> toCBOR (attributes pb)

instance FromCBOR ProposalBody where
  fromCBOR = do
    enforceSize "ProposalBody" 5
    ProposalBody <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Prepend byte corresponding to `encodeListLen 5`, which was used during
--   signing
recoverProposalSignedBytes
  :: Annotated ProposalBody ByteString -> Annotated ProposalBody ByteString
recoverProposalSignedBytes = fmap ("\133" <>)
