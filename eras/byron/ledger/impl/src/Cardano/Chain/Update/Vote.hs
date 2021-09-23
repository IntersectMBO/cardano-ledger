{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Chain.Update.Vote
  ( -- * Vote
    AVote (..),
    Vote,
    VoteId,

    -- * Vote Constructors
    mkVote,
    signVote,
    signatureForVote,
    unsafeVote,

    -- * Vote Accessors
    proposalId,
    recoverVoteId,

    -- * Vote Binary Serialization
    recoverSignedBytes,

    -- * Vote Formatting
    formatVoteShort,
    shortVoteF,
  )
where

import Cardano.Binary
  ( Annotated (Annotated, unAnnotated),
    ByteSpan,
    Decoded (..),
    FromCBOR (..),
    ToCBOR (..),
    annotatedDecoder,
    encodeListLen,
    enforceSize,
    fromCBORAnnotated,
  )
import qualified Cardano.Binary as Binary (annotation)
import Cardano.Chain.Common (addressHash)
import Cardano.Chain.Update.Proposal (Proposal, UpId)
import Cardano.Crypto
  ( Hash,
    ProtocolMagicId,
    SafeSigner,
    SignTag (SignUSVote),
    Signature,
    SigningKey,
    VerificationKey,
    hashDecoded,
    safeSign,
    safeToVerification,
    shortHashF,
    sign,
    toVerification,
  )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, later)
import qualified Formatting.Buildable as B

--------------------------------------------------------------------------------
-- Vote
--------------------------------------------------------------------------------

-- | An update proposal vote identifier (the 'Hash' of a 'Vote').
type VoteId = Hash Vote

type Vote = AVote ()

-- | Vote for update proposal
--
--   Invariant: The signature is valid.
data AVote a = UnsafeVote
  { -- | Verification key casting the vote
    voterVK :: !VerificationKey,
    -- | Proposal to which this vote applies
    aProposalId :: !(Annotated UpId a),
    -- | Signature of (Update proposal, Approval/rejection bit)
    signature :: !(Signature (UpId, Bool)),
    annotation :: !a
  }
  deriving (Eq, Show, Generic, Functor)
  deriving anyclass (NFData)

-- Used for debugging purposes only
instance ToJSON a => ToJSON (AVote a)

--------------------------------------------------------------------------------
-- Vote Constructors
--------------------------------------------------------------------------------

-- | A safe constructor for 'UnsafeVote'
mkVote ::
  ProtocolMagicId ->
  -- | The voter
  SigningKey ->
  -- | Proposal which is voted for
  UpId ->
  -- | Approval/rejection bit
  Bool ->
  Vote
mkVote pm sk upId decision =
  UnsafeVote
    (toVerification sk)
    (Annotated upId ())
    (sign pm SignUSVote sk (upId, decision))
    ()

-- | Create a vote for the given update proposal id, signing it with the
-- provided safe signer.
signVote ::
  ProtocolMagicId ->
  -- | Proposal which is voted for
  UpId ->
  -- | Approval/rejection bit
  Bool ->
  -- | The voter
  SafeSigner ->
  Vote
signVote protocolMagicId upId decision safeSigner =
  unsafeVote
    (safeToVerification safeSigner)
    upId
    (signatureForVote protocolMagicId upId decision safeSigner)

signatureForVote ::
  ProtocolMagicId ->
  UpId ->
  Bool ->
  SafeSigner ->
  Signature (UpId, Bool)
signatureForVote protocolMagicId upId decision safeSigner =
  safeSign protocolMagicId SignUSVote safeSigner (upId, decision)

-- | Create a vote for the given update proposal id using the provided
-- signature.
--
-- For the meaning of the parameters see 'signVote'.
unsafeVote ::
  VerificationKey ->
  UpId ->
  Signature (UpId, Bool) ->
  Vote
unsafeVote vk upId voteSignature =
  UnsafeVote vk (Annotated upId ()) voteSignature ()

--------------------------------------------------------------------------------
-- Vote Accessors
--------------------------------------------------------------------------------

proposalId :: AVote a -> UpId
proposalId = unAnnotated . aProposalId

recoverVoteId :: AVote ByteString -> VoteId
recoverVoteId = hashDecoded

--------------------------------------------------------------------------------
-- Vote Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Vote where
  toCBOR uv =
    encodeListLen 4
      <> toCBOR (voterVK uv)
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
    Annotated (voterVK, aProposalId, signature) byteSpan <- annotatedDecoder $ do
      enforceSize "Vote" 4
      voterVK <- fromCBOR
      aProposalId <- fromCBORAnnotated
      -- Drop the decision bit that previously allowed negative voting
      void $ fromCBOR @Bool
      signature <- fromCBOR
      pure (voterVK, aProposalId, signature)
    pure $ UnsafeVote voterVK aProposalId signature byteSpan

instance Decoded (AVote ByteString) where
  type BaseType (AVote ByteString) = Vote
  recoverBytes = annotation

recoverSignedBytes :: AVote ByteString -> Annotated (UpId, Bool) ByteString
recoverSignedBytes v =
  let bytes =
        mconcat
          [ "\130",
            -- The byte above is part of the signed payload, but is not part of the
            -- transmitted payload
            Binary.annotation $ aProposalId v,
            "\245"
            -- The byte above is the canonical encoding of @True@, which we hardcode,
            -- because we removed the possibility of negative voting
          ]
   in Annotated (proposalId v, True) bytes

--------------------------------------------------------------------------------
-- Vote Formatting
--------------------------------------------------------------------------------

instance B.Buildable (AVote a) where
  build uv =
    bprint
      ( "Update Vote { voter: "
          . build
          . ", proposal id: "
          . build
          . " }"
      )
      (addressHash $ voterVK uv)
      (proposalId uv)

instance B.Buildable (Proposal, [Vote]) where
  build (up, votes) =
    bprint (build . " with votes: " . listJson) up (map formatVoteShort votes)

-- | Format 'Vote' compactly
formatVoteShort :: Vote -> Builder
formatVoteShort uv =
  bprint
    ("(" . shortHashF . " " . shortHashF . ")")
    (addressHash $ voterVK uv)
    (proposalId uv)

-- | Formatter for 'Vote' which displays it compactly
shortVoteF :: Format r (Vote -> r)
shortVoteF = later formatVoteShort
