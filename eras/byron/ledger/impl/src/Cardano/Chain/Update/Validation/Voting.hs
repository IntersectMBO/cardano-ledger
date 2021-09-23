{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Validation rules for registering votes and confirming proposals
--
--   This is an implementation of the rules defined in the Byron ledger
--   specification
module Cardano.Chain.Update.Validation.Voting
  ( Environment (..),
    RegistrationEnvironment (..),
    State (..),
    Error (..),
    registerVoteWithConfirmation,
  )
where

import Cardano.Binary
  ( Annotated,
    Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    matchSize,
  )
import Cardano.Chain.Common (KeyHash, hashKey)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting (SlotNumber)
import Cardano.Chain.Update.Proposal (UpId)
import Cardano.Chain.Update.Vote
  ( AVote (..),
    proposalId,
    recoverSignedBytes,
  )
import Cardano.Crypto
  ( ProtocolMagicId,
    SignTag (SignUSVote),
    verifySignatureDecoded,
  )
import Cardano.Prelude hiding (State)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

-- | Environment used to register votes and confirm proposals
data Environment = Environment
  { veCurrentSlot :: SlotNumber,
    veConfirmationThreshold :: Int,
    veVotingRegistrationEnvironment :: RegistrationEnvironment
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Environment required to validate and register a vote
data RegistrationEnvironment = RegistrationEnvironment
  { vreRegisteredUpdateProposal :: !(Set UpId),
    vreDelegationMap :: !Delegation.Map
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | State keeps track of registered votes and confirmed proposals
data State = State
  { vsVotes :: !RegisteredVotes,
    vsConfirmedProposals :: !(Map UpId SlotNumber)
  }

type RegisteredVotes = Map UpId (Set KeyHash)

-- | Error captures the ways in which vote registration could fail
data Error
  = VotingInvalidSignature
  | VotingProposalNotRegistered UpId
  | VotingVoterNotDelegate KeyHash
  | VotingVoteAlreadyCast KeyHash
  deriving (Eq, Show)

instance ToCBOR Error where
  toCBOR err = case err of
    VotingInvalidSignature ->
      encodeListLen 1
        <> toCBOR (0 :: Word8)
    VotingProposalNotRegistered upId ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR upId
    VotingVoterNotDelegate keyHash ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR keyHash
    VotingVoteAlreadyCast keyHash ->
      encodeListLen 2
        <> toCBOR (3 :: Word8)
        <> toCBOR keyHash

instance FromCBOR Error where
  fromCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size = matchSize "Voting.Error" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 1 >> pure VotingInvalidSignature
      1 -> checkSize 2 >> VotingProposalNotRegistered <$> fromCBOR
      2 -> checkSize 2 >> VotingVoterNotDelegate <$> fromCBOR
      3 -> checkSize 2 >> VotingVoteAlreadyCast <$> fromCBOR
      _ -> cborError $ DecoderErrorUnknownTag "Voting.Error" tag

-- | Register a vote and confirm the corresponding proposal if it passes the
--   voting threshold. This corresponds to the @UPVOTE@ rules in the spec.
registerVoteWithConfirmation ::
  MonadError Error m =>
  Annotated ProtocolMagicId ByteString ->
  Environment ->
  State ->
  AVote ByteString ->
  m State
registerVoteWithConfirmation pm votingEnv vs vote = do
  -- Register the vote ignoring proposal confirmation
  votes' <- registerVote pm voteRegEnv votes vote

  -- Confirm the proposal if it passes the threshold and isn't confirmed
  let confirmedProposals' =
        if pastThreshold votes' && not (isConfirmed upId)
          then M.insert upId slot confirmedProposals
          else confirmedProposals

  -- Return the new state with additional vote and maybe confirmation
  pure $
    State
      { vsVotes = votes',
        vsConfirmedProposals = confirmedProposals'
      }
  where
    Environment slot threshold voteRegEnv = votingEnv
    State votes confirmedProposals = vs

    pastThreshold :: RegisteredVotes -> Bool
    pastThreshold votes' =
      length (M.findWithDefault Set.empty upId votes') >= threshold

    isConfirmed = flip M.member confirmedProposals

    upId = proposalId vote

-- | Validate and register a vote
--
--   We check that
--
--   1) The vote is for a registered proposal
--   2) There is at least one genesis key delegating to the voter
--   3) The signature is valid
--   4) The vote has not already been cast
--
--   This corresponds to the `ADDVOTE` rule in the spec.
registerVote ::
  MonadError Error m =>
  Annotated ProtocolMagicId ByteString ->
  RegistrationEnvironment ->
  RegisteredVotes ->
  AVote ByteString ->
  m RegisteredVotes
registerVote pm vre votes vote = do
  -- Check that the proposal being voted on is registered
  (upId `Set.member` registeredProposals)
    `orThrowError` VotingProposalNotRegistered upId

  -- Check that the set of genesis keys is not empty
  delegator <- case Delegation.lookupR voter delegationMap of
    Nothing -> throwError (VotingVoterNotDelegate voter)
    Just d -> pure d

  -- Check that the vote has not already been cast
  case M.lookup upId votes of
    Just khs | delegator `Set.member` khs -> throwError (VotingVoteAlreadyCast delegator)
    _ -> pure ()

  -- Check that the signature is valid
  verifySignatureDecoded pm SignUSVote voterVK signedBytes signature
    `orThrowError` VotingInvalidSignature

  -- Add the delegators to the set of votes for this proposal
  pure $ M.insertWith Set.union upId (Set.singleton delegator) votes
  where
    RegistrationEnvironment registeredProposals delegationMap = vre

    UnsafeVote {voterVK, signature} = vote

    voter = hashKey voterVK

    upId = proposalId vote

    signedBytes = recoverSignedBytes vote
