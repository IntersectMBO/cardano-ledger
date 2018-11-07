{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Unsafe arbitrary instances for crypto primitives.

module Test.Cardano.Crypto.Arbitrary.Unsafe
  ()
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Coerce (coerce)

import Test.QuickCheck (choose)
import Test.QuickCheck.Instances ()

import qualified Cardano.Binary.Class as Bi
import Cardano.Crypto.Hashing (AbstractHash, HashAlgorithm, abstractHash)
import Cardano.Crypto.Signing (PublicKey, SecretKey)


instance ArbitraryUnsafe PublicKey where
    arbitraryUnsafe = Bi.unsafeDeserialize' . Bi.serialize' <$> arbitrarySizedS 64

instance ArbitraryUnsafe SecretKey where
    arbitraryUnsafe = Bi.unsafeDeserialize' . Bi.serialize' <$> arbitrarySizedS 128

instance HashAlgorithm algo =>
         ArbitraryUnsafe (AbstractHash algo a) where
    arbitraryUnsafe = coerce . abstractHash @algo <$>
        choose (minBound, maxBound :: Word64)
