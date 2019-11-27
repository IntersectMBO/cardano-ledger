{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Cardano.Chain.Update.Vote
  (
  -- * Vote
    Vote(UnsafeVote, voterVK, proposalId, signature, serializeVote)
  , VoteId

  -- * Vote Constructors
  , mkVote
  , signVote
  , signatureForVote

  -- * Vote Accessors
  , aProposalId
  , voteId

  -- * Vote Binary Serialization
  , recoverSignedBytes

  -- * Vote Formatting
  , formatVoteShort
  , shortVoteF
  )
where

import Cardano.Prelude

import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, later)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(Annotated, unAnnotated)
  , FromCBOR(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , encodeListLen
  , encodePreEncoded
  , enforceSize
  , serialize'
  , serializeEncoding'
  , withSlice'
  )
import qualified Cardano.Binary as Binary (annotation)
import Cardano.Chain.Common (addressHash)
import Cardano.Chain.Update.Proposal (Proposal, UpId)
import Cardano.Crypto
  ( Hash
  , ProtocolMagicId
  , VerificationKey
  , SafeSigner
  , SigningKey
  , SignTag(SignUSVote)
  , Signature
  , hash
  , safeSign
  , safeToVerification
  , shortHashF
  , sign
  , toVerification
  )


--------------------------------------------------------------------------------
-- Vote
--------------------------------------------------------------------------------

-- | An update proposal vote identifier (the 'Hash' of a 'Vote').
type VoteId = Hash Vote

-- | Vote for update proposal
--
--   Invariant: The signature is valid.
data Vote = UnsafeVote'
  { voterVK'    :: !VerificationKey
  -- ^ Verification key casting the vote
  , aProposalId':: !(Annotated UpId ByteString)
  -- ^ Proposal to which this vote applies
  , signature'  :: !(Signature (UpId, Bool))
  -- ^ Signature of (Update proposal, Approval/rejection bit)
  , serializeVote  :: ByteString
  } deriving (Generic, Show, Eq)
    deriving anyclass NFData


--------------------------------------------------------------------------------
-- Vote Constructors
--------------------------------------------------------------------------------


-- | Create a vote for the given update proposal id using the provided
-- signature.
--
-- For the meaning of the parameters see 'signVote'.
{-# COMPLETE UnsafeVote #-}
pattern UnsafeVote :: VerificationKey -> UpId -> Signature (UpId, Bool) -> Vote
pattern UnsafeVote { voterVK, proposalId, signature } <- UnsafeVote'
  voterVK
  (unAnnotated -> proposalId)
  signature
  _
  where
  UnsafeVote  vk upId voteSignature =
    let upIdBytes = serialize' upId
        bytes = serializeEncoding' $
          encodeListLen 4
            <> toCBOR vk
            <> encodePreEncoded upIdBytes
            -- We encode @True@ here because we removed the decision bit. This is safe
            -- because we know that all @Vote@s on mainnet use this encoding and any
            -- changes to the encoding in our implementation will be picked up by
            -- golden tests.
            <> toCBOR True
            <> toCBOR voteSignature
    in  UnsafeVote' vk (Annotated upId upIdBytes) voteSignature bytes

-- | A safe constructor for 'UnsafeVote'
mkVote
  :: ProtocolMagicId
  -> SigningKey
  -- ^ The voter
  -> UpId
  -- ^ Proposal which is voted for
  -> Bool
  -- ^ Approval/rejection bit
  -> Vote
mkVote pm sk upId decision = UnsafeVote
  (toVerification sk)
  upId
  (sign pm SignUSVote sk (upId, decision))


-- | Create a vote for the given update proposal id, signing it with the
-- provided safe signer.
signVote
  :: ProtocolMagicId
  -> UpId
  -- ^ Proposal which is voted for
  -> Bool
  -- ^ Approval/rejection bit
  -> SafeSigner
  -- ^ The voter
  -> Vote
signVote protocolMagicId upId decision safeSigner =
  UnsafeVote
    (safeToVerification safeSigner)
    upId
    (signatureForVote protocolMagicId upId decision safeSigner)


signatureForVote
  :: ProtocolMagicId
  -> UpId
  -> Bool
  -> SafeSigner
  -> Signature (UpId, Bool)
signatureForVote protocolMagicId upId decision safeSigner =
  safeSign protocolMagicId SignUSVote safeSigner (upId, decision)




--------------------------------------------------------------------------------
-- Vote Accessors
--------------------------------------------------------------------------------

voteId :: Vote -> VoteId
voteId = hash

aProposalId :: Vote -> Annotated UpId ByteString
aProposalId = aProposalId'

--------------------------------------------------------------------------------
-- Vote Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Vote where
  toCBOR = encodePreEncoded . serializeVote

instance FromCBORAnnotated Vote where
  fromCBORAnnotated' = withSlice' $
    UnsafeVote' <$ lift (enforceSize "Vote" 4)
      <*> lift fromCBOR
      <*> fromCBORAnnotated'
      -- Drop the decision bit that previously allowed negative voting
      <*  lift (fromCBOR @Bool)
      <*> lift fromCBOR

recoverSignedBytes :: Vote -> Annotated (UpId, Bool) ByteString
recoverSignedBytes v =
  let
    bytes = mconcat
      [ "\130"
      -- The byte above is part of the signed payload, but is not part of the
      -- transmitted payload
      , Binary.annotation $ aProposalId v
      , "\245"
      -- The byte above is the canonical encoding of @True@, which we hardcode,
      -- because we removed the possibility of negative voting
      ]
  in Annotated (proposalId v, True) bytes


--------------------------------------------------------------------------------
-- Vote Formatting
--------------------------------------------------------------------------------

instance B.Buildable Vote where
  build uv = bprint
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
formatVoteShort uv = bprint
  ("(" . shortHashF . " " . shortHashF . ")")
  (addressHash $ voterVK uv)
  (proposalId uv)

-- | Formatter for 'Vote' which displays it compactly
shortVoteF :: Format r (Vote -> r)
shortVoteF = later formatVoteShort
