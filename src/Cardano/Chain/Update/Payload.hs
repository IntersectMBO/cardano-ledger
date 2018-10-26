{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.Payload
       ( Payload (..)
       , PayloadError (..)
       , checkPayload
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (..))
import           Formatting (bprint, build)
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Update.Vote (Proposal, ProposalError, Vote,
                     VoteError, checkProposal, checkVote, formatMaybeProposal,
                     formatVoteShort)
import           Cardano.Crypto (ProtocolMagic)


-- | Update System payload
data Payload = Payload
  { payloadProposal :: !(Maybe Proposal)
  , payloadVotes    :: ![Vote]
  -- ^ Votes received on the different update proposals posted so far, not only
  -- on the current @payloadProposal@.
  } deriving (Eq, Show, Generic)

instance NFData Payload

instance B.Buildable Payload where
  build payload
    | null (payloadVotes payload)
    = formatMaybeProposal (payloadProposal payload) <> ", no votes"
    | otherwise
    = formatMaybeProposal (payloadProposal payload) <> bprint
      ("\n    votes: " . listJson)
      (map formatVoteShort (payloadVotes payload))

instance Bi Payload where
  encode payload = encodeListLen 2 <> encode (payloadProposal payload) <> encode
    (payloadVotes payload)

  decode = do
    enforceSize "Update.Payload" 2
    Payload <$> decode <*> decode

data PayloadError
  = PayloadProposalError ProposalError
  | PayloadVoteError VoteError

instance B.Buildable PayloadError where
  build = \case
    PayloadProposalError err -> bprint
      ("Proposal was invalid while checking Update.Payload.\n Error: " . build)
      err
    PayloadVoteError err -> bprint
      ("Vote was invalid while checking Update.Payload.\n Error: " . build)
      err

checkPayload :: MonadError PayloadError m => ProtocolMagic -> Payload -> m ()
checkPayload pm payload = do
  maybe
    (pure ())
    (either (throwError . PayloadProposalError) pure . checkProposal pm)
    (payloadProposal payload)
  forM_
    (payloadVotes payload)
    (either (throwError . PayloadVoteError) pure . checkVote pm)
