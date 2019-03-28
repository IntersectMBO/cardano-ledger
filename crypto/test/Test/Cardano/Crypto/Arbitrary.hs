{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | `Arbitrary` instances for using in tests and benchmarks

module Test.Cardano.Crypto.Arbitrary
  ( genSignature
  , genSignatureEncoded
  , genRedeemSignature
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.ByteArray as ByteArray
import Test.QuickCheck (Arbitrary(..), Gen, elements, oneof, vector)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import Cardano.Binary.Class (Bi)
import Cardano.Crypto.Hashing (AbstractHash(..), HashAlgorithm)
import Cardano.Crypto.HD (HDAddressPayload, HDPassphrase(..))
import Cardano.Crypto.ProtocolMagic
  (ProtocolMagic(..), ProtocolMagicId(..), RequiresNetworkMagic(..))
import Cardano.Crypto.Random (deterministic)
import Cardano.Crypto.Signing
  ( EncryptedSecretKey(..)
  , PassPhrase
  , ProxyCert
  , ProxyVerificationKey
  , ProxySignature
  , PublicKey
  , SafeSigner(..)
  , SecretKey
  , SignTag(..)
  , Signature
  , createPsk
  , emptyPassphrase
  , keyGen
  , noPassEncrypt
  , proxySign
  , safeCreateProxyCert
  , sign
  , signEncoded
  , toPublic
  )
import Cardano.Crypto.Signing.Redeem
  (RedeemPublicKey, RedeemSecretKey, RedeemSignature, redeemKeyGen, redeemSign)

import Test.Cardano.Crypto.Arbitrary.Unsafe ()


instance Arbitrary ProtocolMagic where
    arbitrary = ProtocolMagic <$> arbitrary
                              <*> arbitrary

instance Arbitrary ProtocolMagicId where
    arbitrary = ProtocolMagicId <$> arbitrary

instance Arbitrary RequiresNetworkMagic where
    arbitrary = elements [RequiresNoMagic, RequiresMagic]

{- A note on 'Arbitrary' instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generating keys takes time, so we just pregenerate lots of keys in advance
and use them in 'Arbitrary' instances.
-}

keysToGenerate :: Int
keysToGenerate = 128


--------------------------------------------------------------------------------
-- SignTag
--------------------------------------------------------------------------------

instance Arbitrary SignTag where
    arbitrary = genericArbitrary
    shrink = genericShrink


--------------------------------------------------------------------------------
-- Arbitrary signing keys
--------------------------------------------------------------------------------

-- If you want an arbitrary keypair, just generate a secret key with
-- 'arbitrary' and then use 'Cardano.Crypto.toPublic' to get the corresponding
-- public key.

keys :: [(PublicKey, SecretKey)]
keys = deterministic "keys" $ replicateM keysToGenerate keyGen

instance Arbitrary PublicKey where
    arbitrary = fst <$> elements keys
instance Arbitrary SecretKey where
    arbitrary = snd <$> elements keys

instance Nonrepeating PublicKey where
    nonrepeating n = map fst <$> sublistN n keys
instance Nonrepeating SecretKey where
    nonrepeating n = map snd <$> sublistN n keys

-- Repeat the same for ADA redemption keys
redemptionKeys :: [(RedeemPublicKey, RedeemSecretKey)]
redemptionKeys =
  deterministic "redemptionKeys" $ replicateM keysToGenerate redeemKeyGen

instance Arbitrary RedeemPublicKey where
    arbitrary = fst <$> elements redemptionKeys
instance Arbitrary RedeemSecretKey where
    arbitrary = snd <$> elements redemptionKeys

instance Nonrepeating RedeemPublicKey where
    nonrepeating n = map fst <$> sublistN n redemptionKeys
instance Nonrepeating RedeemSecretKey where
    nonrepeating n = map snd <$> sublistN n redemptionKeys


--------------------------------------------------------------------------------
-- Arbitrary signatures
--------------------------------------------------------------------------------

-- | Generate a signature with a given 'ProtocolMagic', for some generated
-- bytes. The 'SignTag' and 'SecretKey' are generated using their
-- 'Arbitrary' instances.
genSignatureEncoded :: ProtocolMagicId -> Gen ByteString -> Gen (Signature a)
genSignatureEncoded pm genBytestring =
  signEncoded pm <$> arbitrary <*> arbitrary <*> genBytestring

-- | Like 'genSignatureEncoded' but use an 'a' that can be serialized.
genSignature :: Bi a => ProtocolMagicId -> Gen a -> Gen (Signature a)
genSignature pm genA = sign pm <$> arbitrary <*> arbitrary <*> genA

genRedeemSignature
  :: Bi a => ProtocolMagicId -> Gen a -> Gen (RedeemSignature a)
genRedeemSignature pm genA = redeemSign pm <$> arbitrary <*> arbitrary <*> genA

instance (Bi a, Arbitrary a) => Arbitrary (Signature a) where
  arbitrary = do
    pm <- arbitrary
    genSignature pm arbitrary

instance (Bi a, Arbitrary a) => Arbitrary (RedeemSignature a) where
  arbitrary = do
    pm <- arbitrary
    genRedeemSignature pm arbitrary

instance (Bi w, Arbitrary w) => Arbitrary (ProxyCert w) where
    arbitrary = safeCreateProxyCert <$> arbitrary <*> arbitrary
                                    <*> arbitrary <*> arbitrary

instance (Bi w, Arbitrary w) => Arbitrary (ProxyVerificationKey w) where
    arbitrary = createPsk <$> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary

instance (Bi w, Arbitrary w, Bi a, Arbitrary a) =>
         Arbitrary (ProxySignature w a) where
  arbitrary = do
    delegateSk <- arbitrary
    pm <- arbitrary
    psk        <-
      createPsk pm
      <$> arbitrary
      <*> pure (toPublic delegateSk)
      <*> arbitrary
    proxySign pm SignProxyVK delegateSk psk <$> arbitrary


--------------------------------------------------------------------------------
-- Arbitrary hashes
--------------------------------------------------------------------------------

instance (HashAlgorithm algo, Bi a) => Arbitrary (AbstractHash algo a) where
    arbitrary = arbitraryUnsafe


--------------------------------------------------------------------------------
-- Arbitrary passphrases
--------------------------------------------------------------------------------

instance Arbitrary PassPhrase where
    arbitrary = oneof [
        pure mempty,
        ByteArray.pack <$> vector 32
        ]


--------------------------------------------------------------------------------
-- HD
--------------------------------------------------------------------------------

instance Arbitrary HDPassphrase where
    arbitrary = HDPassphrase . toS @[Char] <$> vector 32

instance Arbitrary HDAddressPayload where
    arbitrary = genericArbitrary


--------------------------------------------------------------------------
-- Cardano.Crypto.Signing.Types.Safe
--------------------------------------------------------------------------

instance Arbitrary EncryptedSecretKey where
    arbitrary = noPassEncrypt <$> arbitrary

instance Arbitrary SafeSigner where
  arbitrary = SafeSigner <$> arbitrary <*> pure emptyPassphrase
