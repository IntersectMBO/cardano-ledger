{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test.Cardano.Cbor.CborSpec specification

module Test.Cardano.Crypto.CborSpec
  ( spec
  )
where

import Cardano.Prelude

import Crypto.Hash (Blake2b_224, Blake2b_256)
import Test.Hspec (Spec, describe)

import qualified Cardano.Crypto as Crypto

import Test.Cardano.Binary.Helpers (U, binaryTest)
import Test.Cardano.Crypto.Arbitrary ()


spec :: Spec
spec = describe "Cbor.Bi instances" $ describe "Crypto" $ do
  describe "Hashing" $ binaryTest @(Crypto.Hash Word64)
  describe "Signing" $ describe "Bi instances" $ do
    binaryTest @Crypto.SecretKey
    binaryTest @Crypto.PublicKey
    binaryTest @(Crypto.Signature ())
    binaryTest @(Crypto.Signature U)
    binaryTest @(Crypto.ProxyCert Int32)
    binaryTest @(Crypto.ProxySecretKey Int32)
    binaryTest @(Crypto.ProxySecretKey U)
    binaryTest @(Crypto.ProxySignature Int32 Int32)
    binaryTest @(Crypto.ProxySignature U U)
    binaryTest @(Crypto.Signed Bool)
    binaryTest @(Crypto.Signed U)
    binaryTest @Crypto.RedeemSecretKey
    binaryTest @Crypto.RedeemPublicKey
    binaryTest @(Crypto.RedeemSignature Bool)
    binaryTest @(Crypto.RedeemSignature U)
    binaryTest @Crypto.PassPhrase
    binaryTest @Crypto.HDAddressPayload
    binaryTest @(Crypto.AbstractHash Blake2b_224 U)
    binaryTest @(Crypto.AbstractHash Blake2b_256 U)
