{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.Vote
       (
       -- Software update proposal
         Proposal (..)
       , Proposals
       , UpId
       , ProposalBody (..)
       , formatMaybeProposal
       , signProposal
       , checkProposal

       -- Software update vote
       , VoteId
       , Vote (..)
       , mkVote
       , mkVoteSafe
       , formatVoteShort
       , shortVoteF
       , checkVote
       , mkVoteId
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, bprint, build, builder, later, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Common (addressHash)
import           Cardano.Chain.Common.Attributes (Attributes,
                     areAttributesKnown)
import           Cardano.Chain.Update.BlockVersion (BlockVersion)
import           Cardano.Chain.Update.BlockVersionModifier
                     (BlockVersionModifier)
import           Cardano.Chain.Update.Data (UpdateData)
import           Cardano.Chain.Update.SoftwareVersion (SoftwareVersion,
                     checkSoftwareVersion)
import           Cardano.Chain.Update.SystemTag (SystemTag, checkSystemTag)
import           Cardano.Crypto (Hash, ProtocolMagic, PublicKey, SafeSigner,
                     SecretKey, SignTag (SignUSProposal, SignUSVote),
                     Signature, checkSig, hash, safeSign, safeToPublic,
                     shortHashF, sign, toPublic)


--------------------------------------------------------------------------------
-- Software Update Proposal
--------------------------------------------------------------------------------

-- | ID of software update proposal
type UpId = Hash Proposal

data ProposalBody = ProposalBody
  { pbBlockVersion         :: !BlockVersion
  , pbBlockVersionModifier :: !BlockVersionModifier
  , pbSoftwareVersion      :: !SoftwareVersion
  , pbData                 :: !(Map SystemTag UpdateData)
  -- ^ UpdateData for each system which this update affects. It must be
  --   non-empty.
  , pbAttributes           :: !(Attributes ())
  -- ^ Attributes which are currently empty, but provide extensibility
  } deriving (Eq, Show, Generic)

instance NFData ProposalBody

instance Bi ProposalBody where
  encode pb =
    encodeListLen 5
      <> encode (pbBlockVersion pb)
      <> encode (pbBlockVersionModifier pb)
      <> encode (pbSoftwareVersion pb)
      <> encode (pbData pb)
      <> encode (pbAttributes pb)

  decode = do
    enforceSize "ProposalBody" 5
    ProposalBody <$> decode <*> decode <*> decode <*> decode <*> decode

-- | Proposal for software update
data Proposal = Proposal
  { proposalBody      :: ProposalBody
  , proposalIssuer    :: !PublicKey
  -- ^ Who proposed this UP.
  , proposalSignature :: !(Signature ProposalBody)
  } deriving (Eq, Show, Generic, Typeable)

type Proposals = Map UpId Proposal

instance B.Buildable Proposal where
  build proposal = bprint
    ( build
    % " { block v"
    % build
    % ", UpId: "
    % build
    % ", "
    % build
    % ", tags: "
    % listJson
    % ", "
    % builder
    % " }"
    )
    (pbSoftwareVersion body)
    (pbBlockVersion body)
    (hash proposal)
    (pbBlockVersionModifier body)
    (Map.keys $ pbData body)
    attrsBuilder
   where
    body  = proposalBody proposal
    attrs = pbAttributes body
    attrsBuilder
      | areAttributesKnown attrs = "no attributes"
      | otherwise                = bprint ("attributes: " % build) attrs

instance NFData Proposal

instance Bi Proposal where
  encode proposal =
    encodeListLen 7
      <> encode (pbBlockVersion body)
      <> encode (pbBlockVersionModifier body)
      <> encode (pbSoftwareVersion body)
      <> encode (pbData body)
      <> encode (pbAttributes body)
      <> encode (proposalIssuer proposal)
      <> encode (proposalSignature proposal)
    where body = proposalBody proposal

  decode = do
    enforceSize "Proposal" 7
    Proposal <$> decodeBody <*> decode <*> decode
   where
    decodeBody =
      ProposalBody <$> decode <*> decode <*> decode <*> decode <*> decode

formatMaybeProposal :: Maybe Proposal -> Builder
formatMaybeProposal = maybe "no proposal" B.build

signProposal :: ProtocolMagic -> ProposalBody -> SafeSigner -> Proposal
signProposal pm body ss = Proposal
  { proposalBody      = body
  , proposalIssuer    = issuer
  , proposalSignature = signature
  }
 where
  issuer    = safeToPublic ss
  signature = safeSign pm SignUSProposal ss body

checkProposal :: MonadError Text m => ProtocolMagic -> Proposal -> m ()
checkProposal pm proposal = do
  let body = proposalBody proposal
  checkSoftwareVersion (pbSoftwareVersion body)
  forM_ (Map.keys (pbData body)) checkSystemTag
  let
    sigIsValid = checkSig
      pm
      SignUSProposal
      (proposalIssuer proposal)
      body
      (proposalSignature proposal)
  unless sigIsValid $ throwError "Proposal: invalid signature"


--------------------------------------------------------------------------------
-- Software Update Vote
--------------------------------------------------------------------------------

-- | ID of a voter and its decision regarding a specific software update
--   proposal
type VoteId = (UpId, PublicKey, Bool)

instance B.Buildable VoteId where
  build (upId, pk, dec) = bprint
    ( "Vote Id { voter: "
    % build
    % ", proposal id: "
    % build
    % ", voter's decision: "
    % build
    % " }"
    )
    pk
    upId
    dec

-- | Vote for update proposal
--
--   Invariant: The signature is valid.
data Vote = UnsafeVote
  { uvKey        :: !PublicKey
  -- ^ Public key of stakeholder, who votes
  , uvProposalId :: !UpId
  -- ^ Proposal to which this vote applies
  , uvDecision   :: !Bool
  -- ^ Approval/rejection bit
  , uvSignature  :: !(Signature (UpId, Bool))
  -- ^ Signature of (Update proposal, Approval/rejection bit) by stakeholder
  } deriving (Eq, Show, Generic, Typeable)

instance NFData Vote

instance B.Buildable Vote where
  build uv = bprint
    ( "Update Vote { voter: "
    % build
    % ", proposal id: "
    % build
    % ", voter's decision: "
    % build
    % " }"
    )
    (addressHash $ uvKey uv)
    (uvProposalId uv)
    (uvDecision uv)

instance B.Buildable (Proposal, [Vote]) where
  build (up, votes) =
    bprint (build % " with votes: " % listJson) up (map formatVoteShort votes)

instance Bi Vote where
  encode uv =
    encodeListLen 4
      <> encode (uvKey uv)
      <> encode (uvProposalId uv)
      <> encode (uvDecision uv)
      <> encode (uvSignature uv)

  decode = do
    enforceSize "Vote" 4
    UnsafeVote <$> decode <*> decode <*> decode <*> decode

-- | A safe constructor for 'UnsafeVote'
mkVote
  :: ProtocolMagic
  -> SecretKey
  -- ^ The voter
  -> UpId
  -- ^ Proposal which is voted for
  -> Bool
  -- ^ Approval/rejection bit
  -> Vote
mkVote pm sk proposalId decision = UnsafeVote
  { uvKey        = toPublic sk
  , uvProposalId = proposalId
  , uvDecision   = decision
  , uvSignature  = sign pm SignUSVote sk (proposalId, decision)
  }

-- | Same as 'mkVote', but uses 'SafeSigner'
mkVoteSafe
  :: ProtocolMagic
  -> SafeSigner
  -- ^ The voter
  -> UpId
  -- ^ Proposal which is voted for
  -> Bool
  -- ^ Approval/rejection bit
  -> Vote
mkVoteSafe pm sk proposalId decision = UnsafeVote
  { uvKey        = safeToPublic sk
  , uvProposalId = proposalId
  , uvDecision   = decision
  , uvSignature  = safeSign pm SignUSVote sk (proposalId, decision)
  }

-- | Format 'Vote' compactly
formatVoteShort :: Vote -> Builder
formatVoteShort uv = bprint
  ("(" % shortHashF % " " % builder % " " % shortHashF % ")")
  (addressHash $ uvKey uv)
  (bool "against" "for" $ uvDecision uv)
  (uvProposalId uv)

-- | Formatter for 'Vote' which displays it compactly
shortVoteF :: Format r (Vote -> r)
shortVoteF = later formatVoteShort

checkVote :: (MonadError Text m) => ProtocolMagic -> Vote -> m ()
checkVote pm it = unless sigValid (throwError "Vote: invalid signature")
 where
  sigValid = checkSig
    pm
    SignUSVote
    (uvKey it)
    (uvProposalId it, uvDecision it)
    (uvSignature it)

mkVoteId :: Vote -> VoteId
mkVoteId vote = (uvProposalId vote, uvKey vote, uvDecision vote)
