{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Validation rules for registering votes and confirming proposals
--
--   This is an implementation of the rules defined in the Byron ledger
--   specification
module Cardano.Chain.Update.Validation.Voting
  ( Environment(..)
  , RegistrationEnvironment(..)
  , State(..)
  , Error(..)
  , registerVoteWithConfirmation
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Cardano.Binary (Annotated)
import Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import Cardano.Chain.Slotting (FlatSlotId)
import Cardano.Chain.Update.Proposal (UpId)
import Cardano.Chain.Update.Vote
  ( AVote(..)
  , recoverSignedBytes
  , proposalId
  )
import Cardano.Crypto
  ( ProtocolMagicId
  , SignTag(SignUSVote)
  , verifySignatureDecoded
  )


-- | Environment used to register votes and confirm proposals
data Environment = Environment
  { veCurrentSlot                   :: FlatSlotId
  , veConfirmationThreshold         :: Int
  , veVotingRegistrationEnvironment :: RegistrationEnvironment
  }

-- | Environment required to validate and register a vote
data RegistrationEnvironment = RegistrationEnvironment
 { vreRegisteredUpdateProposal :: Set UpId
 , vreDelegationMap            :: Map StakeholderId StakeholderId
 }

-- | State keeps track of registered votes and confirmed proposals
data State = State
  { vsVotes              :: RegisteredVotes
  , vsConfirmedProposals :: Map UpId FlatSlotId
  }

type RegisteredVotes = Map UpId (Set StakeholderId)

-- | Error captures the ways in which vote registration could fail
data Error
  = VotingInvalidSignature
  | VotingProposalNotRegistered UpId
  | VotingVoterNotDelegate StakeholderId
  deriving (Eq, Show)


-- | Register a vote and confirm the corresponding proposal if it passes the
--   voting threshold. This corresponds to the @UPVOTE@ rules in the spec.
registerVoteWithConfirmation
  :: MonadError Error m
  => Annotated ProtocolMagicId ByteString
  -> Environment
  -> State
  -> AVote ByteString
  -> m State
registerVoteWithConfirmation pm votingEnv vs vote = do

  -- Register the vote ignoring proposal confirmation
  votes' <- registerVote pm voteRegEnv votes vote

  -- Confirm the proposal if it passes the threshold and isn't confirmed
  let
    confirmedProposals' = if pastThreshold votes' && not (isConfirmed upId)
      then M.insert upId slot confirmedProposals
      else confirmedProposals

  -- Return the new state with additional vote and maybe confirmation
  pure $ State
    { vsVotes = votes'
    , vsConfirmedProposals = confirmedProposals'
    }
 where
  Environment slot threshold voteRegEnv  = votingEnv
  State votes confirmedProposals = vs

  pastThreshold :: RegisteredVotes -> Bool
  pastThreshold votes' =
    length (M.findWithDefault Set.empty upId votes') >= threshold

  isConfirmed = flip M.member confirmedProposals

  upId        = proposalId vote


-- | Validate and register a vote
--
--   We check that
--
--   1) The vote is for a registered proposal
--   2) There is at least one genesis key delegating to the voter
--   3) The signature is valid
--
--   This corresponds to the `ADDVOTE` rule in the spec.
registerVote
  :: MonadError Error m
  => Annotated ProtocolMagicId ByteString
  -> RegistrationEnvironment
  -> RegisteredVotes
  -> AVote ByteString
  -> m RegisteredVotes
registerVote pm vre votes vote = do

  -- Check that the proposal being voted on is registered
  (upId `Set.member` registeredProposals)
    `orThrowError` VotingProposalNotRegistered upId

  -- Retrieve the set of genesis keys delegating to the voter
  let delegators = Set.fromList . M.keys $ M.filter (== voter) delegationMap

  -- Check that the set of genesis keys is not empty
  not (null delegators) `orThrowError` VotingVoterNotDelegate voter

  -- Check that the signature is valid
  verifySignatureDecoded pm SignUSVote voterVK signedBytes signature
    `orThrowError` VotingInvalidSignature

  -- Add the delegators to the set of votes for this proposal
  pure $ M.insertWith Set.union upId delegators votes
 where
  RegistrationEnvironment registeredProposals delegationMap = vre

  UnsafeVote { voterVK, signature } = vote

  voter       = mkStakeholderId voterVK

  upId        = proposalId vote

  signedBytes = recoverSignedBytes vote
