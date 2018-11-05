{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Round-trip testing for the @Bi@ instances defined in this package
--
--   TODO: Move this property testing to `hedgehog` rather than `hspec`

module Test.Cardano.Binary.Cbor.CborSpec
       ( spec
       , U
       , extensionProperty
       ) where

import           Cardano.Prelude

import           Data.Bits
    (shiftL)
import qualified Data.ByteString as BS
import           Data.String
    (String)

import           Test.Hspec
    (Spec, describe)
import           Test.Hspec.QuickCheck
    (modifyMaxSize, modifyMaxSuccess, prop)
import           Test.QuickCheck
    (Arbitrary (..), choose, sized, (===))

import           Cardano.Binary.Class

import           Test.Cardano.Binary.Helpers
    (U, extensionProperty)
import qualified Test.Cardano.Cbor.RefImpl as R


-- | Wrapper for Integer with Arbitrary instance that can generate "proper" big
-- integers, i.e. ones that don't fit in Int64. This really needs to be fixed
-- within QuickCheck though (https://github.com/nick8325/quickcheck/issues/213).
newtype LargeInteger =
  LargeInteger Integer
  deriving (Eq, Show)

instance Arbitrary LargeInteger where
  arbitrary = sized $ \sz -> do
    n    <- choose (1, sz)
    sign <- arbitrary
    LargeInteger
      .   (if sign then negate else identity)
      .   foldr f 0
      <$> replicateM n arbitrary
   where
    f :: Word8 -> Integer -> Integer
    f w acc = (acc `shiftL` 8) + fromIntegral w

instance Bi LargeInteger where
  encode (LargeInteger n) = encode n
  decode = LargeInteger <$> decode

----------------------------------------

data User
  = Login String Int
  | FullName String String Bool
  deriving (Show, Eq)

instance Bi User where
  encode = \case
    Login s i -> encodeListLen 3
      <> encode (0 :: Word8)
      <> encode s
      <> encode i
    FullName f l b -> encodeListLen 4
      <> encode (1 :: Word8)
      <> encode f
      <> encode l
      <> encode b

  decode = do
    actualSize <- decodeListLenCanonical
    decode >>= \case
      0 -> do
        matchSize "Login :: User" 3 actualSize
        Login <$> decode <*> decode
      1 -> do
        matchSize "FullName :: User" 4 actualSize
        FullName <$> decode <*> decode <*> decode
      t -> cborError $ DecoderErrorUnknownTag "User" t

----------------------------------------

data T
  = T1 Int
  | T2 Int Int
  | Unknown Word8 BS.ByteString
  deriving (Show)

instance Bi T where
  encode = \case
    T1 a         -> encode (0 :: Word8) <> encode (serialize' a)
    T2      a b  -> encode (1 :: Word8) <> encode (serialize' (a, b))
    Unknown n bs -> encode n <> encode bs

  decode = decode @Word8 >>= \case
    0 -> T1 <$> (deserialize' =<< decode)
    1 -> uncurry T2 <$> (deserialize' =<< decode)
    t -> Unknown t <$> decode

----------------------------------------

spec :: Spec
spec = do
  describe "Cbor.Bi instances" $ modifyMaxSuccess (const 1000) $ prop
    "User"
    (let u1 = Login "asd" 34 in unsafeDeserialize (serialize u1) === u1)

  describe "Reference implementation" $ do
    describe "properties" $ do
      prop "encoding/decoding initial byte"    R.prop_InitialByte
      prop "encoding/decoding additional info" R.prop_AdditionalInfo
      prop "encoding/decoding token header"    R.prop_TokenHeader
      prop "encoding/decoding token header 2"  R.prop_TokenHeader2
      prop "encoding/decoding tokens"          R.prop_Token
      modifyMaxSuccess (const 1000) . modifyMaxSize (const 150) $
        prop "encoding/decoding terms" R.prop_Term
    describe "internal properties" $ do
      prop "Integer to/from bytes"             R.prop_integerToFromBytes
      prop "Word16 to/from network byte order" R.prop_word16ToFromNet
      prop "Word32 to/from network byte order" R.prop_word32ToFromNet
      prop "Word64 to/from network byte order" R.prop_word64ToFromNet
      modifyMaxSuccess (const 1) $
        -- Using once inside the property would be lovely (as it tests
        -- all the Halfs) but it doesn't work for some reason.
        prop "Numeric.Half to/from Float" R.prop_halfToFromFloat
