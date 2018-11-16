{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Crypto.Signing.Tag
  ( signTag
  , module Cardano.Crypto.Signing.Types.Tag
  )
where

import Cardano.Prelude

import qualified Cardano.Binary.Class as Bi
import Cardano.Crypto.ProtocolMagic (ProtocolMagic(..))
import Cardano.Crypto.Signing.Types.Tag


-- | Get magic bytes corresponding to a 'SignTag'. Guaranteed to be different
-- (and begin with a different byte) for different tags.
signTag :: ProtocolMagic -> SignTag -> ByteString
signTag protocolMagic = \case
  SignForTestingOnly -> "\x00"
  SignTx             -> "\x01" <> network
  SignRedeemTx       -> "\x02" <> network
  SignVssCert        -> "\x03" <> network
  SignUSProposal     -> "\x04" <> network
  SignCommitment     -> "\x05" <> network
  SignUSVote         -> "\x06" <> network
  SignMainBlock      -> "\x07" <> network
  -- "\x08" was used for SignMainBlockLight, but was never used in mainnet
  SignMainBlockHeavy -> "\x09" <> network
  SignProxySK        -> "\x0a" <> network
  where network = Bi.serialize' (getProtocolMagic protocolMagic)
