{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Cardano.Chain.Update.Vote
  (
  -- * Software update proposal
    AProposal(..)
  , Proposal
  , ProposalError(..)
  , UpId
  , ProposalBody(..)
  , checkProposal
  , formatMaybeProposal
  , mkProposal
  , proposalBody
  , recoverProposalSignedBytes
  , recoverUpId
  , signProposal

  -- * Software update vote
  , AVote(..)
  , VoteId
  , Vote
  , proposalId
  , mkVote
  , mkVoteSafe
  , formatVoteShort
  , shortVoteF
  , mkVoteId
  , recoverSignedBytes
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import qualified Data.Map.Strict as Map
import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, builder, later)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan
  , Decoded(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , annotatedDecoder
  , fromCBORAnnotated
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Common (addressHash)
import Cardano.Chain.Common.Attributes (Attributes, areAttributesKnown)
import Cardano.Chain.Update.Data (UpdateData)
import Cardano.Chain.Update.ProtocolParametersUpdate (ProtocolParametersUpdate)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion
  (SoftwareVersion, SoftwareVersionError, checkSoftwareVersion)
import Cardano.Chain.Update.SystemTag
  (SystemTag, SystemTagError, checkSystemTag)
import Cardano.Crypto
  ( Hash
  , ProtocolMagicId
  , PublicKey
  , SafeSigner
  , SecretKey
  , SignTag(SignUSProposal, SignUSVote)
  , Signature
  , hash
  , hashDecoded
  , safeSign
  , safeToPublic
  , shortHashF
  , sign
  , toPublic
  , verifySignatureDecoded
  )


--------------------------------------------------------------------------------
-- Software Update Proposal
--------------------------------------------------------------------------------

-- | ID of software update proposal
type UpId = Hash Proposal

data ProposalBody = ProposalBody
  { pbProtocolVersion          :: !ProtocolVersion
  , pbProtocolParametersUpdate :: !ProtocolParametersUpdate
  , pbSoftwareVersion          :: !SoftwareVersion
  , pbData                     :: !(Map SystemTag UpdateData)
  -- ^ UpdateData for each system which this update affects. It must be
  --   non-empty.
  , pbAttributes               :: !(Attributes ())
  -- ^ Attributes which are currently empty, but provide extensibility
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance ToCBOR ProposalBody where
  toCBOR pb =
    encodeListLen 5
      <> toCBOR (pbProtocolVersion pb)
      <> toCBOR (pbProtocolParametersUpdate pb)
      <> toCBOR (pbSoftwareVersion pb)
      <> toCBOR (pbData pb)
      <> toCBOR (pbAttributes pb)

instance FromCBOR ProposalBody where
  fromCBOR = do
    enforceSize "ProposalBody" 5
    ProposalBody <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Prepend byte corresponding to `encodeListLen 5`, which was used during
--   signing
recoverProposalSignedBytes
  :: Annotated ProposalBody ByteString -> Annotated ProposalBody ByteString
recoverProposalSignedBytes = fmap ("\133" <>)

-- | Proposal for software update
data AProposal a = AProposal
  { aProposalBody      :: !(Annotated ProposalBody a)
  , proposalIssuer     :: !PublicKey
  -- ^ Who proposed this UP.
  , proposalSignature  :: !(Signature ProposalBody)
  , proposalAnnotation :: !a
  } deriving (Eq, Show, Generic, Functor)
    deriving anyclass NFData

type Proposal = AProposal ()

mkProposal :: ProposalBody -> PublicKey -> Signature ProposalBody -> Proposal
mkProposal b k s = AProposal (Annotated b ()) k s ()

proposalBody :: AProposal a -> ProposalBody
proposalBody = unAnnotated . aProposalBody

recoverUpId :: AProposal ByteString -> UpId
recoverUpId = hashDecoded

instance Decoded (AProposal ByteString) where
  type BaseType (AProposal ByteString) = Proposal
  recoverBytes = proposalAnnotation

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
    (pbSoftwareVersion body)
    (pbProtocolVersion body)
    (hash proposal)
    (pbProtocolParametersUpdate body)
    (Map.keys $ pbData body)
    attrsBuilder
   where
    body  = proposalBody proposal
    attrs = pbAttributes body
    attrsBuilder
      | areAttributesKnown attrs = "no attributes"
      | otherwise                = bprint ("attributes: " . build) attrs


instance ToCBOR Proposal where
  toCBOR proposal =
    encodeListLen 7
      <> toCBOR (pbProtocolVersion body)
      <> toCBOR (pbProtocolParametersUpdate body)
      <> toCBOR (pbSoftwareVersion body)
      <> toCBOR (pbData body)
      <> toCBOR (pbAttributes body)
      <> toCBOR (proposalIssuer proposal)
      <> toCBOR (proposalSignature proposal)
    where body = proposalBody proposal

instance FromCBOR Proposal where
  fromCBOR = void <$> fromCBOR @(AProposal ByteSpan)

instance FromCBOR (AProposal ByteSpan) where
  fromCBOR = do
    Annotated (body, pk, sig) byteSpan <- annotatedDecoder $ do
      enforceSize "Proposal" 7
      body <- annotatedDecoder
        (ProposalBody <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR)
      pk   <- fromCBOR
      sig  <- fromCBOR
      pure (body, pk, sig)
    pure $ AProposal body pk sig byteSpan

formatMaybeProposal :: Maybe Proposal -> Builder
formatMaybeProposal = maybe "no proposal" B.build

signProposal :: ProtocolMagicId -> ProposalBody -> SafeSigner -> Proposal
signProposal pm body ss = mkProposal body issuer sig
 where
  issuer = safeToPublic ss
  sig    = safeSign pm SignUSProposal ss body

data ProposalError
  = ProposalInvalidSignature (Signature ProposalBody)
  | ProposalSoftwareVersionError SoftwareVersionError
  | ProposalSystemTagError SystemTagError

instance B.Buildable ProposalError where
  build = \case
    ProposalInvalidSignature sig ->
      bprint ("Invalid signature, " . build . ", in Proposal") sig
    ProposalSoftwareVersionError err -> bprint
      ("SoftwareVersion was invalid while checking Proposal.\n Error: " . build)
      err
    ProposalSystemTagError err -> bprint
      ("SystemTag was invalid while checking Proposal.\n Error: " . build)
      err

-- TODO: See if we need to use any of these internal checks
checkProposal
  :: MonadError ProposalError m
  => ProtocolMagicId
  -> AProposal ByteString
  -> m ()
checkProposal pm proposal = do
  let
    aBody = aProposalBody proposal
    body  = unAnnotated aBody
  liftEither . first ProposalSoftwareVersionError $ checkSoftwareVersion
    (pbSoftwareVersion body)
  forM_
    (Map.keys (pbData body))
    (liftEither . first ProposalSystemTagError . checkSystemTag)
  let
    bodyPrefix = "\133" :: ByteString -- toLazyByteString $ encodeListLen 5
    sigIsValid = verifySignatureDecoded
      pm
      SignUSProposal
      (proposalIssuer proposal)
      (mappend bodyPrefix <$> aBody)
      (proposalSignature proposal)
  sigIsValid
    `orThrowError` ProposalInvalidSignature (proposalSignature proposal)


--------------------------------------------------------------------------------
-- Software Update Vote
--------------------------------------------------------------------------------

-- | ID of a voter and its decision regarding a specific software update
--   proposal
type VoteId = (UpId, PublicKey, Bool)

instance B.Buildable VoteId where
  build (id, pk, dec) = bprint
    ( "Vote Id { voter: "
    . build
    . ", proposal id: "
    . build
    . ", voter's decision: "
    . build
    . " }"
    )
    pk
    id
    dec


type Vote = AVote ()
-- | Vote for update proposal
--
--   Invariant: The signature is valid.
data AVote a = UnsafeVote
  { voterPK       :: !PublicKey
  -- ^ Public key of stakeholder, who votes
  , aProposalId   :: !(Annotated UpId a)
  -- ^ Proposal to which this vote applies
  , signature     :: !(Signature (UpId, Bool))
  -- ^ Signature of (Update proposal, Approval/rejection bit) by stakeholder
  } deriving (Eq, Show, Generic, Functor)
    deriving anyclass NFData

proposalId :: AVote a -> UpId
proposalId = unAnnotated . aProposalId

recoverSignedBytes :: AVote ByteString -> Annotated (UpId, Bool) ByteString
recoverSignedBytes v =
  let
    bytes = mconcat
      [ "\130"
      -- The byte above is part of the signed payload, but is not part of the transmitted payload.
      -- This is an implementation artifact of the previous
      , annotation $ aProposalId v
      , "\245"
      -- The byte above is the canonical encoding of @True@, which we hardcode,
      -- because we removed the possibility of negative voting
      ]
  in Annotated (proposalId v, True) bytes

instance B.Buildable (AVote a) where
  build uv = bprint
    ( "Update Vote { voter: "
    . build
    . ", proposal id: "
    . build
    . " }"
    )
    (addressHash $ voterPK uv)
    (proposalId uv)

instance B.Buildable (Proposal, [Vote]) where
  build (up, votes) =
    bprint (build . " with votes: " . listJson) up (map formatVoteShort votes)

instance ToCBOR Vote where
  toCBOR uv =
    encodeListLen 4
      <> toCBOR (voterPK uv)
      <> toCBOR (proposalId uv)
      -- We encode @True@ here because we removed the decision bit. This is safe
      -- because we know that all @Vote@s on mainnet use this encoding and any
      -- changes to the encoding in our implementation will be picked up by
      -- golden tests.
      <> toCBOR True
      <> toCBOR (signature uv)

instance FromCBOR Vote where
  fromCBOR = void <$> fromCBOR @(AVote ByteSpan)

instance FromCBOR (AVote ByteSpan) where
  fromCBOR = do
    enforceSize "Vote" 4
    voterPK     <- fromCBOR
    aProposalId <- fromCBORAnnotated
    -- Drop the decision bit that previously allowed negative voting
    void $ fromCBOR @Bool
    signature <- fromCBOR
    pure $ UnsafeVote { voterPK, aProposalId, signature }

-- | A safe constructor for 'UnsafeVote'
mkVote
  :: ProtocolMagicId
  -> SecretKey
  -- ^ The voter
  -> UpId
  -- ^ Proposal which is voted for
  -> Bool
  -- ^ Approval/rejection bit
  -> Vote
mkVote pm sk upId decision = UnsafeVote
  (toPublic sk)
  (Annotated upId ())
  (sign pm SignUSVote sk (upId, decision))

-- | Same as 'mkVote', but uses 'SafeSigner'
mkVoteSafe
  :: ProtocolMagicId
  -> SafeSigner
  -- ^ The voter
  -> UpId
  -- ^ Proposal which is voted for
  -> Bool
  -- ^ Approval/rejection bit
  -> Vote
mkVoteSafe pm sk upId decision = UnsafeVote
  (safeToPublic sk)
  (Annotated upId ())
  (safeSign pm SignUSVote sk (upId, decision))

-- | Format 'Vote' compactly
formatVoteShort :: Vote -> Builder
formatVoteShort uv = bprint
  ("(" . shortHashF . " " . shortHashF . ")")
  (addressHash $ voterPK uv)
  (proposalId uv)

-- | Formatter for 'Vote' which displays it compactly
shortVoteF :: Format r (Vote -> r)
shortVoteF = later formatVoteShort

mkVoteId :: Vote -> VoteId
mkVoteId vote = (proposalId vote, voterPK vote, True)
