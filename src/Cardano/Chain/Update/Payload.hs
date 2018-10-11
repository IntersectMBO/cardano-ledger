{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.Payload
       ( Payload (..)
       , checkPayload
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError)
import           Formatting (bprint, (%))
import           Formatting.Buildable (Buildable (..))

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Update.Vote (Proposal, Vote, checkProposal,
                     checkVote, formatMaybeProposal, formatVoteShort)
import           Cardano.Crypto (ProtocolMagic)


-- | Update System payload
data Payload = Payload
  { payloadProposal :: !(Maybe Proposal)
  , payloadVotes    :: ![Vote]
  } deriving (Eq, Show, Generic, Typeable)

instance NFData Payload

instance Buildable Payload where
  build payload
    | null (payloadVotes payload)
    = formatMaybeProposal (payloadProposal payload) <> ", no votes"
    | otherwise
    = formatMaybeProposal (payloadProposal payload) <> bprint
      ("\n    votes: " % listJson)
      (map formatVoteShort (payloadVotes payload))

instance Bi Payload where
  encode payload = encodeListLen 2 <> encode (payloadProposal payload) <> encode
    (payloadVotes payload)

  decode = do
    enforceSize "Update.Payload" 2
    Payload <$> decode <*> decode

checkPayload :: MonadError Text m => ProtocolMagic -> Payload -> m ()
checkPayload pm payload = do
  whenJust (payloadProposal payload) (checkProposal pm)
  forM_ (payloadVotes payload) (checkVote pm)
