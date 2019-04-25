{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Cardano.Chain.Update.Vote
  (
  -- * Vote
    AVote(..)
  , Vote

  -- * Vote Constructors
  , mkVote
  , mkVoteSafe

  -- * Vote Accessors
  , proposalId

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
  ( Annotated(..)
  , ByteSpan
  , FromCBOR(..)
  , ToCBOR(..)
  , fromCBORAnnotated
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Common (addressHash)
import Cardano.Chain.Update.Proposal (Proposal, UpId)
import Cardano.Crypto
  ( ProtocolMagicId
  , PublicKey
  , SafeSigner
  , SecretKey
  , SignTag(SignUSVote)
  , Signature
  , safeSign
  , safeToPublic
  , shortHashF
  , sign
  , toPublic
  )


--------------------------------------------------------------------------------
-- Vote
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Vote Constructors
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Vote Accessors
--------------------------------------------------------------------------------

proposalId :: AVote a -> UpId
proposalId = unAnnotated . aProposalId


--------------------------------------------------------------------------------
-- Vote Binary Serialization
--------------------------------------------------------------------------------

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

recoverSignedBytes :: AVote ByteString -> Annotated (UpId, Bool) ByteString
recoverSignedBytes v =
  let
    bytes = mconcat
      [ "\130"
      -- The byte above is part of the signed payload, but is not part of the
      -- transmitted payload
      , annotation $ aProposalId v
      , "\245"
      -- The byte above is the canonical encoding of @True@, which we hardcode,
      -- because we removed the possibility of negative voting
      ]
  in Annotated (proposalId v, True) bytes


--------------------------------------------------------------------------------
-- Vote Formatting
--------------------------------------------------------------------------------

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

-- | Format 'Vote' compactly
formatVoteShort :: Vote -> Builder
formatVoteShort uv = bprint
  ("(" . shortHashF . " " . shortHashF . ")")
  (addressHash $ voterPK uv)
  (proposalId uv)

-- | Formatter for 'Vote' which displays it compactly
shortVoteF :: Format r (Vote -> r)
shortVoteF = later formatVoteShort
