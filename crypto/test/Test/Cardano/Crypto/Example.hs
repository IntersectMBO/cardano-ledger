{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Crypto.Example
       ( examplePublicKey
       , examplePublicKeys
       , exampleRedeemPublicKey
       , exampleSecretKey
       , exampleSecretKeys
       , exampleSafeSigner
       , staticSafeSigners
       ) where

import           Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import           Data.List ((!!))
import           Data.Maybe (fromJust)

import           Cardano.Crypto (PublicKey (..), RedeemPublicKey,
                     SafeSigner (..), SecretKey (..),
                     redeemDeterministicKeyGen)

import           Test.Cardano.Crypto.Bi (getBytes)


examplePublicKey :: PublicKey
examplePublicKey = pk
  where [pk] = examplePublicKeys 16 1 -- 16 could be any number, as we take the first key

examplePublicKeys :: Int -> Int -> [PublicKey]
examplePublicKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right pk = PublicKey <$> CC.xpub (getBytes start 64)
                   in pk

exampleRedeemPublicKey :: RedeemPublicKey
exampleRedeemPublicKey = fromJust (fst <$> redeemDeterministicKeyGen (getBytes 0 32))

-- In order to get the key starting at byte 10, we generate two with offsets of 10
-- between them and take the second.
exampleSecretKey :: SecretKey
exampleSecretKey = exampleSecretKeys 10 2 !! 1

exampleSecretKeys :: Int -> Int -> [SecretKey]
exampleSecretKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right sk = SecretKey <$> CC.xprv (getBytes start 128)
                   in sk

exampleSafeSigner :: Int -> SafeSigner
exampleSafeSigner offset = staticSafeSigners !! offset

staticSafeSigners :: [SafeSigner]
staticSafeSigners = map FakeSigner (exampleSecretKeys 1 6)
