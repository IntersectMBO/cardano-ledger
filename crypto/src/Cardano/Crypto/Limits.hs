{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Crypto.Limits
  ( mlAbstractHash
  , mlVerificationKey
  , mlXSignature
  , mlSignature
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import Crypto.Hash.IO (HashAlgorithm, hashDigestSize)

import Cardano.Binary (Limit)
import Cardano.Crypto (AbstractHash, VerificationKey, Signature(..))


mlAbstractHash
  :: forall algo a . HashAlgorithm algo => Limit (AbstractHash algo a)
mlAbstractHash =
  fromIntegral (hashDigestSize (panic "AbstractHash limit" :: algo) + 4)

mlVerificationKey :: Limit VerificationKey
mlVerificationKey = 66

mlXSignature :: Limit CC.XSignature
mlXSignature = 66

mlSignature :: Limit (Signature a)
mlSignature = Signature <$> mlXSignature
