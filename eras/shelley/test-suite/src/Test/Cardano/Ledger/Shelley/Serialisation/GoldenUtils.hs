{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Serialisation.GoldenUtils
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
import Cardano.Ledger.Serialization (ToCBORGroup (..))
import Cardano.Prelude (LByteString)
import Codec.CBOR.Encoding (Encoding (..), Tokens (..))
import Codec.CBOR.Term (decodeTerm)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.String (fromString)
import GHC.Stack
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

roundTrip ::
  (HasCallStack, Show a, Eq a) =>
  (a -> Encoding) ->
  (LByteString -> Either DecoderError a) ->
  a ->
  Assertion
roundTrip encode decode x =
  case (decode . serializeEncoding . encode) x of
    Left e -> assertFailure $ "could not decode serialization of " ++ show x ++ ", " ++ show e
    Right y -> y @?= x

checkEncoding ::
  (HasCallStack, Show a, Eq a) =>
  (a -> Encoding) ->
  (LByteString -> Either DecoderError a) ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncoding encode decode name x t =
  testCase testName $ do
    unless (expectedBinary == actualBinary) $ do
      expectedTerms <- getTerms "expected" expectedBinary
      actualTerms <- getTerms "actual" actualBinary
      assertFailure $
        unlines
          [ "Serialization did not match: ",
            "expected = ",
            show expectedTerms,
            "actual = ",
            show actualTerms
          ]
    roundTrip encode decode x
  where
    getTerms lbl = either throwIO pure . decodeFullDecoder lbl decodeTerm
    expectedBinary = serialize t
    actualBinary = serializeEncoding $ encode x
    testName = "golden_serialize_" <> name

checkEncodingCBOR ::
  (HasCallStack, FromCBOR a, ToCBOR a, Show a, Eq a) =>
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBOR name x t =
  let d = decodeFullDecoder (fromString name) fromCBOR
   in checkEncoding toCBOR d name x t

checkEncodingCBORAnnotated ::
  (HasCallStack, FromCBOR (Annotator a), ToCBOR a, Show a, Eq a) =>
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
