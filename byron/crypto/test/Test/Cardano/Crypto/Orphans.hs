{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Crypto.Orphans () where

import Cardano.Crypto
  ( SigningKey (..),
  )
import Cardano.Crypto.Hashing (serializeCborHash)
import Cardano.Prelude
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as BA

-- Note that we /only/ provide these Eq and Ord instances for test suites.
-- The crypto libraries encourage using key /hashes/ not keys for things
-- like sets, map etc.

instance Eq SigningKey where
  a == b = serializeCborHash a == serializeCborHash b

instance Ord Ed25519.PublicKey where
  compare x1 x2 = compare (toByteString x1) (toByteString x2)

instance Ord Ed25519.SecretKey where
  compare x1 x2 = compare (toByteString x1) (toByteString x2)

instance Ord Ed25519.Signature where
  compare x1 x2 = compare (toByteString x1) (toByteString x2)

toByteString :: (BA.ByteArrayAccess bin) => bin -> ByteString
toByteString = BA.convert
