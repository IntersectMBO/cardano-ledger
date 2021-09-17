{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Crypto.Signing.Tag
  ( SignTag (..),
    signTag,
    signTagDecoded,
  )
where

import Cardano.Binary (Annotated (..), serialize')
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import Cardano.Crypto.Signing.VerificationKey (VerificationKey (..))
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Formatting (bprint, shown)
import Formatting.Buildable (Buildable (..))

-- | To protect against replay attacks (i.e. when an attacker intercepts a
--   signed piece of data and later sends it again), we add a tag to all data
--   that we sign. This ensures that even if some bytestring can be deserialized
--   into two different types of messages (A and B), the attacker can't take
--   message A and send it as message B.
--
--   We also automatically add the network tag ('protocolMagic') whenever it
--   makes sense, to ensure that things intended for testnet won't work for
--   mainnet.
data SignTag
  = -- | Anything (to be used for testing only)
    SignForTestingOnly
  | -- | Tx:               @TxSigData@
    SignTx
  | -- | Redeem tx:        @TxSigData@
    SignRedeemTx
  | -- | Vss certificate:  @(VssVerificationKey, EpochNumber)@
    SignVssCert
  | -- | Update proposal:  @UpdateProposalToSign@
    SignUSProposal
  | -- | Commitment:       @(EpochNumber, Commitment)@
    SignCommitment
  | -- | US proposal vote: @(UpId, Bool)@
    SignUSVote
  | -- | Block header:     @ToSign@
    --
    --   This constructor takes the 'VerificationKey' of the delegation
    --   certificate issuer, which is prepended to the signature as part of the
    --   sign tag
    SignBlock VerificationKey
  | -- | Certificate:      @Certificate@
    SignCertificate
  deriving (Eq, Ord, Show, Generic)

-- TODO: it would be nice if we couldn't use 'SignTag' with wrong
-- types. Maybe something with GADTs and data families?

instance Buildable SignTag where
  build = bprint shown

-- | Get magic bytes corresponding to a 'SignTag', taking `ProtocolMagic` bytes
--   from the annotation
signTagDecoded :: Annotated ProtocolMagicId ByteString -> SignTag -> ByteString
signTagDecoded = signTagRaw . annotation

-- | Get magic bytes corresponding to a 'SignTag'. Guaranteed to be different
--   (and begin with a different byte) for different tags.
signTag :: ProtocolMagicId -> SignTag -> ByteString
signTag = signTagRaw . serialize' . unProtocolMagicId

signTagRaw :: ByteString -> SignTag -> ByteString
signTagRaw network = \case
  SignForTestingOnly -> "\x00"
  SignTx -> "\x01" <> network
  SignRedeemTx -> "\x02" <> network
  SignVssCert -> "\x03" <> network
  SignUSProposal -> "\x04" <> network
  SignCommitment -> "\x05" <> network
  SignUSVote -> "\x06" <> network
  -- "\x07" was used for SignMainBlock, but was never used in mainnet
  -- "\x08" was used for SignMainBlockLight, but was never used in mainnet

  -- This tag includes the prefix that was previously added in @proxySign@,
  -- allowing us to unify the two signing functions
  SignBlock (VerificationKey issuerVK) ->
    "01" <> CC.unXPub issuerVK <> "\x09" <> network
  SignCertificate -> "\x0a" <> network
