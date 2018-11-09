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

import Test.QuickCheck (Arbitrary(..), choose)
import Test.QuickCheck.Instances ()

import Cardano.Binary.Class (Bi)
import qualified Cardano.Binary.Class as Bi
import Cardano.Crypto.Hashing (AbstractHash, HashAlgorithm, abstractHash)
import Cardano.Crypto.Signing (PublicKey, SecretKey, SignTag, Signed, mkSigned)

import Test.Cardano.Crypto.Dummy (dummyProtocolMagic)


instance ArbitraryUnsafe PublicKey where
    arbitraryUnsafe = Bi.unsafeDeserialize' . Bi.serialize' <$> arbitrarySizedS 64

instance ArbitraryUnsafe SecretKey where
    arbitraryUnsafe = Bi.unsafeDeserialize' . Bi.serialize' <$> arbitrarySizedS 128

-- Generating invalid `Signed` objects doesn't make sense even in
-- benchmarks
instance (Bi a, ArbitraryUnsafe a, Arbitrary SignTag) =>
         ArbitraryUnsafe (Signed a) where
    arbitraryUnsafe = mkSigned <$> pure dummyProtocolMagic
                               <*> arbitrary
                               <*> arbitraryUnsafe
                               <*> arbitraryUnsafe

instance HashAlgorithm algo =>
         ArbitraryUnsafe (AbstractHash algo a) where
    arbitraryUnsafe = coerce . abstractHash @algo <$>
        choose (minBound, maxBound :: Word64)
