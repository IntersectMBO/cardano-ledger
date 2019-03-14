{-# LANGUAGE FlexibleContexts #-}

-- | Validation rules for registering votes and confirming proposals
--
--   This is an implementation of the rules defined in the Byron ledger
--   specification
module Cardano.Chain.Update.Validation.Voting
  ( VotingEnvironment(..)
  , VoteRegistrationEnvironment(..)
  , VotingState(..)
  , VotingError(..)
  , registerVoteWithConfirmation
  )
where

import Cardano.Prelude

import qualified Data.Map as M
import qualified Data.Set as Set

import Cardano.Chain.Common
import Cardano.Chain.Slotting
import Cardano.Chain.Update.ProtocolParameters
import Cardano.Chain.Update.Vote
import Cardano.Crypto


-- | Environment used to register votes and confirm proposals
data VotingEnvironment = VotingEnvironment
  { veCurrentSlot                   :: FlatSlotId
  , veProtocolParameters            :: ProtocolParameters
  , veVotingRegistrationEnvironment :: VoteRegistrationEnvironment
  }

-- | Environment required to validate and register a vote
data VoteRegistrationEnvironment = VoteRegistrationEnvironment
 { vreRegisteredUpdateProposal :: Set UpId
 , vreDelegationMap            :: Map StakeholderId StakeholderId
 }

-- | VotingState keeps track of registered votes and confirmed proposals
data VotingState = VotingState
  { vsVotes              :: RegisteredVotes
  , vsConfirmedProposals :: Map UpId FlatSlotId
  }

type RegisteredVotes = Map UpId (Set StakeholderId)

-- | VotingError captures the ways in which vote registration could fail
data VotingError
  = VotingInvalidSignature
  | VotingProposalNotRegistered UpId
  | VotingVoterNotDelegate StakeholderId


-- | Register a vote and confirm the corresponding proposal if it passes the
--   voting threshold. This corresponds to the `UPVOTE` rules in the spec.
registerVoteWithConfirmation
  :: MonadError VotingError m
  => ProtocolMagicId
  -> VotingEnvironment
  -> VotingState
  -> AVote ByteString
  -> m VotingState
registerVoteWithConfirmation pm votingEnv vs vote = do

  -- Register the vote ignoring proposal confirmation
  votes' <- registerVote pm voteRegEnv votes vote

  -- Confirm the proposal if it passes the threshold and isn't confirmed
  let
    confirmedProposals' = if pastThreshold votes' && not (isConfirmed upId)
      then M.insert upId slot confirmedProposals
      else confirmedProposals

  -- Return the new state with additional vote and maybe confirmation
  pure $ VotingState
    { vsVotes = votes'
    , vsConfirmedProposals = confirmedProposals'
    }
 where
  VotingEnvironment slot _ voteRegEnv  = votingEnv
  VotingState votes confirmedProposals = vs

  -- | This is the number of genesis keys that need to support a proposal
  threshold :: Int
  threshold = 5

  pastThreshold :: RegisteredVotes -> Bool
  pastThreshold votes' =
    length (M.findWithDefault Set.empty upId votes') >= threshold

  isConfirmed = flip M.member confirmedProposals

  upId        = uvProposalId vote


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
  :: MonadError VotingError m
  => ProtocolMagicId
  -> VoteRegistrationEnvironment
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
  verifySignatureDecoded pm SignUSVote voterPK signedBytes signature
    `orThrowError` VotingInvalidSignature

  -- Add the delegators to the set of votes for this proposal
  pure $ M.insertWith Set.union upId delegators votes
 where
  VoteRegistrationEnvironment registeredProposals delegationMap = vre

  voterPK     = uvKey vote
  voter       = mkStakeholderId voterPK

  upId        = uvProposalId vote

  signature   = uvSignature vote
  signedBytes = recoverSignedBytes vote
