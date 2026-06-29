{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.Vintage.Helpers (
  byronProtVer,

  -- * Binary test helpers
  U,
  U24,
  extensionProperty,
  cborFlatTermValid,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  byronProtVer,
  decodeListLenOf,
  decodeNestedCborBytes,
  encodeListLen,
  encodeNestedCborBytes,
  serialize,
  unsafeDeserialize,
 )
import Cardano.Ledger.Binary.FlatTerm (toFlatTerm, validFlatTerm)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Hspec ()
import Test.Hspec.QuickCheck ()
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  Property,
  choose,
  forAll,
  property,
  (===),
 )

--------------------------------------------------------------------------------
-- From/to tests
--------------------------------------------------------------------------------

-- | Machinery to test we perform "flat" encoding.
cborFlatTermValid :: EncCBOR a => a -> Property
cborFlatTermValid = property . validFlatTerm . toFlatTerm byronProtVer . encCBOR

--------------------------------------------------------------------------------

-- Type to be used to simulate a breaking change in the serialisation
-- schema, so we can test instances which uses the `UnknownXX` pattern
-- for extensibility.
-- Check the `extensionProperty` for more details.
data U = U Word8 BS.ByteString deriving (Show, Eq)

instance EncCBOR U where
  encCBOR (U word8 bs) =
    encodeListLen 2
      <> encCBOR (word8 :: Word8)
      <> encodeNestedCborBytes
        (LBS.fromStrict bs)

instance DecCBOR U where
  decCBOR = do
    decodeListLenOf 2
    U <$> decCBOR <*> decodeNestedCborBytes

instance Arbitrary U where
  arbitrary = U <$> choose (0, 255) <*> arbitrary

-- | Like `U`, but we expect to read back the Cbor Data Item when decoding.
data U24 = U24 Word8 BS.ByteString deriving (Show, Eq)

instance DecCBOR U24 where
  decCBOR = do
    decodeListLenOf 2
    U24 <$> decCBOR <*> decodeNestedCborBytes

instance EncCBOR U24 where
  encCBOR (U24 word8 bs) =
    encodeListLen 2
      <> encCBOR (word8 :: Word8)
      <> encodeNestedCborBytes
        (LBS.fromStrict bs)

-- | Given a data type which can be extended, verify we can indeed do so
-- without breaking anything. This should work with every time which adopted
-- the schema of having at least one constructor of the form:
-- .... | Unknown Word8 ByteString
extensionProperty ::
  forall a. (Arbitrary a, Eq a, Show a, DecCBOR a, EncCBOR a) => Property
extensionProperty = forAll @a (arbitrary :: Gen a) $ \input ->
  {- This function works as follows:

     1. When we call `serialized`, we are implicitly assuming (as contract of this
        function) that the input type would be of a shape such as:

        data MyType = Constructor1 Int Bool
                    | Constructor2 String
                    | UnknownConstructor Word8 ByteString

        Such type will be encoded, roughly, like this:

        encode (Constructor1 a b) = encodeWord 0 <> encodeNestedCbor (a,b)
        encode (Constructor2 a b) = encodeWord 1 <> encodeNestedCbor a
        encode (UnknownConstructor tag bs) = encodeWord tag <> encodeNestedCborBytes bs

        In CBOR terms, we would produce something like this:

        <tag :: Word32><Tag24><CborDataItem :: ByteString>

     2. Now, when we call `unsafeDeserialize serialized`, we are effectively asking to produce as
        output a value of type `U`. `U` is defined by only 1 constructor, it
        being `U Word8 ByteString`, but this is still compatible with our `tag + cborDataItem`
        format. So now we will have something like:

        U <tag :: Word32> <CborDataItem :: ByteString>

        (The <Tag24> has been removed as part of the decoding process).

     3. We now call `unsafeDeserialize (serialize u)`, which means: Can you produce a CBOR binary
        from `U`, and finally try to decode it into a value of type `a`? This will work because
        our intermediate encoding into `U` didn't touch the inital `<tag :: Word32>`, so we will
        be able to reconstruct the original object back.
        More specifically, `serialize u` would produce once again:

        <tag :: Word32><Tag24><CborDataItem :: ByteString>

        (The <Tag24> has been added as part of the encoding process).

        `unsafeDeserialize` would then consume the tag (to understand which type constructor this corresponds to),
        remove the <Tag24> token and finally proceed to deserialise the rest.

  -}
  let serialized = serialize byronProtVer input -- Step 1
      (u :: U) = unsafeDeserialize byronProtVer serialized -- Step 2
      (encoded :: a) = unsafeDeserialize byronProtVer (serialize byronProtVer u) -- Step 3
   in encoded === input
