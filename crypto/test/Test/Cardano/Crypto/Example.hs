{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Crypto.Example
  ( exampleProtocolMagic0
  , exampleProtocolMagic1
  , exampleProtocolMagic2
  , exampleProtocolMagic3
  , exampleProtocolMagic4
  , exampleProtocolMagicId0
  , examplePublicKey
  , examplePublicKeys
  , exampleRedeemPublicKey
  , exampleSecretKey
  , exampleSecretKeys
  , exampleSafeSigner
  , staticSafeSigners
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import Data.List ((!!))
import Data.Maybe (fromJust)

import Cardano.Crypto
  ( ProtocolMagic(..)
  , ProtocolMagicId(..)
  , PublicKey(..)
  , RedeemPublicKey
  , RequiresNetworkMagic(..)
  , SafeSigner
  , SecretKey(..)
  , noPassSafeSigner
  , redeemDeterministicKeyGen
  )

import Test.Cardano.Crypto.Bi (getBytes)

exampleProtocolMagicId0 :: ProtocolMagicId
exampleProtocolMagicId0 = ProtocolMagicId 31337

exampleProtocolMagic0 :: ProtocolMagic
exampleProtocolMagic0 = ProtocolMagic exampleProtocolMagicId0 RequiresMagic

exampleProtocolMagic1 :: ProtocolMagic
exampleProtocolMagic1 =
  ProtocolMagic (ProtocolMagicId 2147000001) RequiresMagic

exampleProtocolMagic2 :: ProtocolMagic
exampleProtocolMagic2 = ProtocolMagic (ProtocolMagicId (-58952)) RequiresMagic

exampleProtocolMagic3 :: ProtocolMagic
exampleProtocolMagic3 = ProtocolMagic (ProtocolMagicId 31337) RequiresMagic

exampleProtocolMagic4 :: ProtocolMagic
exampleProtocolMagic4 = ProtocolMagic (ProtocolMagicId (-500)) RequiresNoMagic

examplePublicKey :: PublicKey
examplePublicKey = pk where [pk] = examplePublicKeys 16 1 -- 16 could be any number, as we take the first key

examplePublicKeys :: Int -> Int -> [PublicKey]
examplePublicKeys offset count = map (toKey . (* offset)) [0 .. count - 1]
 where
  toKey start =
    let Right pk = PublicKey <$> CC.xpub (getBytes start 64) in pk

exampleRedeemPublicKey :: RedeemPublicKey
exampleRedeemPublicKey =
  fromJust (fst <$> redeemDeterministicKeyGen (getBytes 0 32))

-- In order to get the key starting at byte 10, we generate two with offsets of 10
-- between them and take the second.
exampleSecretKey :: SecretKey
exampleSecretKey = exampleSecretKeys 10 2 !! 1

exampleSecretKeys :: Int -> Int -> [SecretKey]
exampleSecretKeys offset count = map (toKey . (* offset)) [0 .. count - 1]
 where
  toKey start =
    let Right sk = SecretKey <$> CC.xprv (getBytes start 128) in sk

exampleSafeSigner :: Int -> SafeSigner
exampleSafeSigner offset = staticSafeSigners !! offset

staticSafeSigners :: [SafeSigner]
staticSafeSigners = map noPassSafeSigner (exampleSecretKeys 1 6)
