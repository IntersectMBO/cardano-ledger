{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Cardano.Chain.Update.Vote
  (
       -- Software update proposal
    AProposal(..)
  , Proposal
  , ProposalError(..)
  , Proposals
  , UpId
  , ProposalBody(..)
  , checkProposal
  , decodeAProposal
  , formatMaybeProposal
  , mkProposal
  , proposalBody
  , recoverUpId
  , signProposal

       -- Software update vote
  , AVote(..)
  , VoteId
  , Vote
  , VoteError(..)
  , mkVote
  , mkVoteSafe
  , formatVoteShort
  , shortVoteF
  , checkVote
  , decodeAVote
  , mkVoteId
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import qualified Data.Map.Strict as Map
import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, builder, later)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  ( Annotated(..)
  , Bi(..)
  , ByteSpan
  , Decoded(..)
  , Decoder
  , annotatedDecoder
  , decodeAnnotated
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Common (addressHash)
import Cardano.Chain.Common.Attributes (Attributes, areAttributesKnown)
import Cardano.Chain.Update.BlockVersion (BlockVersion)
import Cardano.Chain.Update.BlockVersionModifier (BlockVersionModifier)
import Cardano.Chain.Update.Data (UpdateData)
import Cardano.Chain.Update.SoftwareVersion
  (SoftwareVersion, SoftwareVersionError, checkSoftwareVersion)
import Cardano.Chain.Update.SystemTag
  (SystemTag, SystemTagError, checkSystemTag)
import Cardano.Crypto
  ( Hash
  , ProtocolMagic
  , PublicKey
  , SafeSigner
  , SecretKey
  , SignTag(SignUSProposal, SignUSVote)
  , Signature
  , verifySignatureDecoded
  , hash
  , hashDecoded
  , safeSign
  , safeToPublic
  , shortHashF
  , sign
  , toPublic
  )


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
data AProposal a = AProposal
  { aProposalBody      :: (Annotated ProposalBody a)
  , proposalIssuer     :: !PublicKey
  -- ^ Who proposed this UP.
  , proposalSignature  :: !(Signature ProposalBody)
  , proposalAnnotation :: !a
  } deriving (Eq, Show, Generic, Functor)

instance NFData a => NFData (AProposal a)

type Proposal = AProposal ()

mkProposal :: ProposalBody -> PublicKey -> (Signature ProposalBody) -> Proposal
mkProposal b k s = AProposal (Annotated b ()) k s ()

proposalBody :: AProposal a -> ProposalBody
proposalBody = unAnnotated . aProposalBody

upId :: Proposal -> UpId
upId = hash

recoverUpId :: AProposal ByteString -> UpId
recoverUpId = hashDecoded

instance Decoded (AProposal ByteString) where
  type BaseType (AProposal ByteString) = Proposal
  recoverBytes = proposalAnnotation

type Proposals = Map UpId Proposal


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
    (pbBlockVersion body)
    (upId proposal)
    (pbBlockVersionModifier body)
    (Map.keys $ pbData body)
    attrsBuilder
   where
    body  = proposalBody proposal
    attrs = pbAttributes body
    attrsBuilder
      | areAttributesKnown attrs = "no attributes"
      | otherwise                = bprint ("attributes: " . build) attrs


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

  decode = void <$> decodeAProposal

decodeAProposal :: Decoder s (AProposal ByteSpan)
decodeAProposal = do
  Annotated (body, pk, signature) byteSpan <- annotatedDecoder $ do
    enforceSize "Proposal" 7
    body <- annotatedDecoder
      (ProposalBody <$> decode <*> decode <*> decode <*> decode <*> decode)
    pk        <- decode
    signature <- decode
    pure (body, pk, signature)
  pure $ AProposal body pk signature byteSpan

formatMaybeProposal :: Maybe Proposal -> Builder
formatMaybeProposal = maybe "no proposal" B.build

signProposal :: ProtocolMagic -> ProposalBody -> SafeSigner -> Proposal
signProposal pm body ss = mkProposal body issuer signature
 where
  issuer    = safeToPublic ss
  signature = safeSign pm SignUSProposal ss body

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

checkProposal
  :: MonadError ProposalError m => ProtocolMagic -> AProposal ByteString -> m ()
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
  { uvKey         :: !PublicKey
  -- ^ Public key of stakeholder, who votes
  , aUvProposalId :: !(Annotated UpId a)
  -- ^ Proposal to which this vote applies
  , aUvDecision   :: !(Annotated Bool a)
  -- ^ Approval/rejection bit
  , uvSignature   :: !(Signature (UpId, Bool))
  -- ^ Signature of (Update proposal, Approval/rejection bit) by stakeholder
  } deriving (Eq, Show, Generic, Functor)

uvProposalId :: AVote a -> UpId
uvProposalId = unAnnotated . aUvProposalId

uvDecision :: AVote a -> Bool
uvDecision = unAnnotated . aUvDecision

recoverSignedBytes :: AVote ByteString -> Annotated (UpId, Bool) ByteString
recoverSignedBytes v =
  let
    bytes = mconcat
      [ "\130"
      -- ^ This byte is part of the signed payload, but is not part of the transmitted payload.
      -- This is an implementation artifact of the previous
      , annotation $ aUvProposalId v
      , annotation $ aUvDecision v
      ]
  in Annotated (uvProposalId v, uvDecision v) bytes

instance NFData a => NFData (AVote a)

instance B.Buildable (AVote a) where
  build uv = bprint
    ( "Update Vote { voter: "
    . build
    . ", proposal id: "
    . build
    . ", voter's decision: "
    . build
    . " }"
    )
    (addressHash $ uvKey uv)
    (uvProposalId uv)
    (uvDecision uv)

instance B.Buildable (Proposal, [Vote]) where
  build (up, votes) =
    bprint (build . " with votes: " . listJson) up (map formatVoteShort votes)

instance Bi Vote where
  encode uv =
    encodeListLen 4
      <> encode (uvKey uv)
      <> encode (uvProposalId uv)
      <> encode (uvDecision uv)
      <> encode (uvSignature uv)

  decode = void <$> decodeAVote

decodeAVote :: Decoder s (AVote ByteSpan)
decodeAVote = do
  enforceSize "Vote" 4
  UnsafeVote <$> decode <*> decodeAnnotated <*> decodeAnnotated <*> decode


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
  (toPublic sk)
  (Annotated proposalId ())
  (Annotated decision ())
  (sign pm SignUSVote sk (proposalId, decision))

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
  (safeToPublic sk)
  (Annotated proposalId ())
  (Annotated decision ())
  (safeSign pm SignUSVote sk (proposalId, decision))

-- | Format 'Vote' compactly
formatVoteShort :: Vote -> Builder
formatVoteShort uv = bprint
  ("(" . shortHashF . " " . builder . " " . shortHashF . ")")
  (addressHash $ uvKey uv)
  (bool "against" "for" $ uvDecision uv)
  (uvProposalId uv)

-- | Formatter for 'Vote' which displays it compactly
shortVoteF :: Format r (Vote -> r)
shortVoteF = later formatVoteShort

data VoteError =
  VoteInvalidSignature (Signature (UpId, Bool))

instance B.Buildable VoteError where
  build = \case
    VoteInvalidSignature sig ->
      bprint ("Invalid signature, " . build . ", in Vote") sig

checkVote :: MonadError VoteError m => ProtocolMagic -> AVote ByteString -> m ()
checkVote pm uv = sigValid `orThrowError` VoteInvalidSignature (uvSignature uv)
 where
  sigValid = verifySignatureDecoded
    pm
    SignUSVote
    (uvKey uv)
    (recoverSignedBytes uv)
    (uvSignature uv)

mkVoteId :: Vote -> VoteId
mkVoteId vote = (uvProposalId vote, uvKey vote, uvDecision vote)
