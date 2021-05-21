{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Serialisation.GoldenUtils
  ( checkEncoding,
    checkEncodingCBOR,
    checkEncodingCBORAnnotated,
    ToTokens (..),
  )
where

import Cardano.Binary
  ( Annotator,
    DecoderError,
    FromCBOR (..),
    ToCBOR (..),
    decodeAnnotator,
    decodeFullDecoder,
    serialize,
    serialize',
    serializeEncoding,
    toCBOR,
  )
import Cardano.Prelude (LByteString)
import Codec.CBOR.Encoding (Encoding (..), Tokens (..))
import qualified Data.ByteString.Base16.Lazy as Base16
import Data.String (fromString)
import Cardano.Ledger.Serialization (ToCBORGroup (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase, (@?=))

roundTrip ::
  (Show a, Eq a) =>
  (a -> Encoding) ->
  (LByteString -> Either DecoderError a) ->
  a ->
  Assertion
roundTrip encode decode x =
  case (decode . serializeEncoding . encode) x of
    Left e -> assertFailure $ "could not decode serialization of " ++ show x ++ ", " ++ show e
    Right y -> y @?= x

checkEncoding ::
  (Show a, Eq a) =>
  (a -> Encoding) ->
  (LByteString -> Either DecoderError a) ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncoding encode decode name x t =
  testCase testName $
    assertEqual
      testName
      (Base16.encode $ serialize t)
      (Base16.encode . serializeEncoding . encode $ x)
      >> roundTrip encode decode x
  where
    testName = "golden_serialize_" <> name

checkEncodingCBOR ::
  (FromCBOR a, ToCBOR a, Show a, Eq a) =>
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBOR name x t =
  let d = decodeFullDecoder (fromString name) fromCBOR
   in checkEncoding toCBOR d name x t

checkEncodingCBORAnnotated ::
  (FromCBOR (Annotator a), ToCBOR a, Show a, Eq a) =>
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBORAnnotated name x t =
  let d = decodeAnnotator (fromString name) fromCBOR
   in checkEncoding toCBOR d name x annTokens
  where
    annTokens = T $ TkEncoded $ serialize' t

data ToTokens where
  T :: (Tokens -> Tokens) -> ToTokens
  S :: ToCBOR a => a -> ToTokens
  G :: ToCBORGroup a => a -> ToTokens
  Plus :: ToTokens -> ToTokens -> ToTokens

instance ToCBOR ToTokens where
  toCBOR (T xs) = Encoding xs
  toCBOR (S s) = toCBOR s
  toCBOR (G g) = toCBORGroup g
  toCBOR (Plus a b) = toCBOR a <> toCBOR b

instance Semigroup ToTokens where
  (<>) = Plus

instance Monoid ToTokens where
  mempty = T id
