{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Crypto.Example
  ( exampleProtocolMagic0,
    exampleProtocolMagic1,
    exampleProtocolMagic2,
    exampleProtocolMagic3,
    exampleProtocolMagic4,
    exampleProtocolMagicId0,
    exampleVerificationKey,
    exampleVerificationKeys,
    exampleRedeemVerificationKey,
    exampleSigningKey,
    exampleSigningKeys,
    exampleSafeSigner,
    staticSafeSigners,
  )
where

import Cardano.Binary (Annotated (..))
import Cardano.Crypto
  ( AProtocolMagic (..),
    ProtocolMagic,
    ProtocolMagicId (..),
    RedeemVerificationKey,
    RequiresNetworkMagic (..),
    SafeSigner,
    SigningKey (..),
    VerificationKey (..),
    noPassSafeSigner,
    redeemDeterministicKeyGen,
  )
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Data.List ((!!))
import Data.Maybe (fromJust)
import Test.Cardano.Crypto.CBOR (getBytes)

exampleProtocolMagicId0 :: ProtocolMagicId
exampleProtocolMagicId0 = ProtocolMagicId 31337

exampleProtocolMagic0 :: ProtocolMagic
exampleProtocolMagic0 =
  AProtocolMagic (Annotated exampleProtocolMagicId0 ()) RequiresMagic

exampleProtocolMagic1 :: ProtocolMagic
exampleProtocolMagic1 =
  AProtocolMagic (Annotated (ProtocolMagicId 2147000001) ()) RequiresMagic

exampleProtocolMagic2 :: ProtocolMagic
exampleProtocolMagic2 =
  AProtocolMagic (Annotated (ProtocolMagicId 58952) ()) RequiresMagic

exampleProtocolMagic3 :: ProtocolMagic
exampleProtocolMagic3 =
  AProtocolMagic (Annotated (ProtocolMagicId 31337) ()) RequiresMagic

exampleProtocolMagic4 :: ProtocolMagic
exampleProtocolMagic4 =
  AProtocolMagic (Annotated (ProtocolMagicId 500) ()) RequiresNoMagic

exampleVerificationKey :: VerificationKey
exampleVerificationKey = vk where [vk] = exampleVerificationKeys 16 1 -- 16 could be any number, as we take the first key

exampleVerificationKeys :: Int -> Int -> [VerificationKey]
exampleVerificationKeys offset count = map (toKey . (* offset)) [0 .. count - 1]
  where
    toKey start =
      let Right vk = VerificationKey <$> CC.xpub (getBytes start 64) in vk

exampleRedeemVerificationKey :: RedeemVerificationKey
exampleRedeemVerificationKey =
  fromJust (fst <$> redeemDeterministicKeyGen (getBytes 0 32))

-- In order to get the key starting at byte 10, we generate two with offsets of 10
-- between them and take the second.
exampleSigningKey :: SigningKey
exampleSigningKey = exampleSigningKeys 10 2 !! 1

exampleSigningKeys :: Int -> Int -> [SigningKey]
exampleSigningKeys offset count = map (toKey . (* offset)) [0 .. count - 1]
  where
    toKey start =
      let Right sk = SigningKey <$> CC.xprv (getBytes start 128) in sk

exampleSafeSigner :: Int -> SafeSigner
exampleSafeSigner offset = staticSafeSigners !! offset

staticSafeSigners :: [SafeSigner]
staticSafeSigners = map noPassSafeSigner (exampleSigningKeys 1 6)
