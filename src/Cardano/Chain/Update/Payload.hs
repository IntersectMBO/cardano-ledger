{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Cardano.Chain.Update.Payload
  ( APayload(..)
  , Payload
  , PayloadError(..)
  , payload
  , checkPayload
  , decodeAPayload
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  ( Annotated(..)
  , Bi(..)
  , ByteSpan
  , Decoded(..)
  , Decoder
  , annotatedDecoder
  , decodeListWith
  , decodeMaybe
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Update.Vote
  ( AProposal
  , AVote
  , Proposal
  , ProposalError
  , Vote
  , VoteError
  , checkProposal
  , checkVote
  , decodeAProposal
  , decodeAVote
  , formatMaybeProposal
  , formatVoteShort
  )
import Cardano.Crypto (ProtocolMagicId)

-- | Update System payload
data APayload a = APayload
  { payloadProposal   :: !(Maybe (AProposal a))
  , payloadVotes      :: ![AVote a]
  , payloadAnnotation :: a
  } deriving (Eq, Show, Generic, Functor)

instance NFData a => NFData (APayload a)

type Payload = APayload ()

payload :: Maybe Proposal -> [Vote] -> Payload
payload p v = APayload p v ()

instance Decoded (APayload ByteString) where
  type BaseType (APayload ByteString) = Payload
  recoverBytes = payloadAnnotation

instance B.Buildable (Payload) where
  build p
    | null (payloadVotes p)
    = formatMaybeProposal (payloadProposal p) <> ", no votes"
    | otherwise
    = formatMaybeProposal (payloadProposal p) <> bprint
      ("\n    votes: " . listJson)
      (map formatVoteShort (payloadVotes p))

instance Bi (APayload ()) where
  encode p = encodeListLen 2 <> encode (payloadProposal p) <> encode (payloadVotes p)

  decode = void <$> decodeAPayload


decodeAPayload :: Decoder s (APayload ByteSpan)
decodeAPayload = do
  Annotated (proposal, votes) byteSpan <- annotatedDecoder $ do
    enforceSize "Update.Payload" 2
    proposal <- decodeMaybe decodeAProposal
    votes    <- decodeListWith decodeAVote
    pure (proposal, votes)
  pure $ APayload proposal votes byteSpan


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

checkPayload
  :: MonadError PayloadError m => ProtocolMagicId -> APayload ByteString -> m ()
checkPayload pm p = do
  maybe
    (pure ())
    (liftEither . first PayloadProposalError . checkProposal pm)
    (payloadProposal p)
  forM_ (payloadVotes p) (liftEither . first PayloadVoteError . checkVote pm)
