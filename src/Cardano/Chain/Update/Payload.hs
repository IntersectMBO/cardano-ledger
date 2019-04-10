{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Cardano.Chain.Update.Payload
  ( APayload(..)
  , Payload
  , PayloadError(..)
  , payload
  , checkPayload
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan
  , Decoded(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , annotatedDecoder
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Update.Vote
  ( AProposal
  , AVote
  , Proposal
  , ProposalError
  , Vote
  , checkProposal
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
    deriving anyclass NFData

type Payload = APayload ()

payload :: Maybe Proposal -> [Vote] -> Payload
payload p v = APayload p v ()

instance Decoded (APayload ByteString) where
  type BaseType (APayload ByteString) = Payload
  recoverBytes = payloadAnnotation

instance B.Buildable Payload where
  build p
    | null (payloadVotes p)
    = formatMaybeProposal (payloadProposal p) <> ", no votes"
    | otherwise
    = formatMaybeProposal (payloadProposal p) <> bprint
      ("\n    votes: " . listJson)
      (map formatVoteShort (payloadVotes p))

instance ToCBOR Payload where
  toCBOR p =
    encodeListLen 2 <> toCBOR (payloadProposal p) <> toCBOR (payloadVotes p)

instance FromCBOR Payload where
  fromCBOR = void <$> fromCBOR @(APayload ByteSpan)

instance FromCBOR (APayload ByteSpan) where
  fromCBOR = do
    Annotated (proposal, votes) byteSpan <- annotatedDecoder $ do
      enforceSize "Update.Payload" 2
      (,) <$> fromCBOR <*> fromCBOR
    pure $ APayload proposal votes byteSpan

newtype PayloadError
  = PayloadProposalError ProposalError

instance B.Buildable PayloadError where
  build = \case
    PayloadProposalError err -> bprint
      ("Proposal was invalid while checking Update.Payload.\n Error: " . build)
      err

checkPayload
  :: MonadError PayloadError m => ProtocolMagicId -> APayload ByteString -> m ()
checkPayload pm p =
  maybe
    (pure ())
    (liftEither . first PayloadProposalError . checkProposal pm)
    (payloadProposal p)
