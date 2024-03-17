{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Shelley.Serialisation.GoldenUtils (
  checkEncoding,
  checkEncodingCBOR,
  checkEncodingCBORAnnotated,
  ToTokens (..),
  roundTripFailure,
  checkEncodingCBORDecodeFailure,
)
where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecoderError,
  EncCBOR (..),
  EncCBORGroup (..),
  Encoding,
  ToCBOR (..),
  Tokens (..),
  Version,
  decodeFullAnnotator,
  decodeFullDecoder,
  decodeTerm,
  encCBOR,
  fromPlainEncoding,
  serialize,
  serialize',
 )

-- ToExpr (CBOR.Term) instance
import qualified Codec.CBOR.Encoding as CBOR (Encoding (..))
import Control.Exception (throwIO)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import Data.String (fromString)
import GHC.Stack
import Test.Cardano.Ledger.Binary.TreeDiff (diffExpr)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

expectDecodingSuccess :: (HasCallStack, Show a, Eq a) => (a -> Either DecoderError a) -> a -> IO ()
expectDecodingSuccess action x =
  case action x of
    Left e -> assertFailure $ "could not decode serialization of " ++ show x ++ ", " ++ show e
    Right y -> y @?= x

expectDecodingFailure :: (HasCallStack, Show a) => (a -> Either DecoderError a) -> a -> IO ()
expectDecodingFailure action x =
  case action x of
    Left _ -> pure ()
    Right _ -> assertFailure $ "Did not expect successful decoding of " ++ show x

roundtrip ::
  Version ->
  (a -> Encoding) ->
  (BSL.ByteString -> Either DecoderError a) ->
  a ->
  Either DecoderError a
roundtrip v encode decode = decode . serialize v . encode

roundTripSuccess ::
  (Show a, Eq a) =>
  Version ->
  (a -> Encoding) ->
  (BSL.ByteString -> Either DecoderError a) ->
  a ->
  Assertion
roundTripSuccess v encode decode x = expectDecodingSuccess (roundtrip v encode decode) x

roundTripFailure ::
  Show a => Version -> (a -> Encoding) -> (BSL.ByteString -> Either DecoderError a) -> a -> Assertion
roundTripFailure v encode decode x = expectDecodingFailure (roundtrip v encode decode) x

checkEncoding ::
  (HasCallStack, Show a, Eq a) =>
  Version ->
  (a -> Encoding) ->
  (BSL.ByteString -> Either DecoderError a) ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncoding v encode decode name x t = checkEncodingWithRoundtrip v encode decode roundTripSuccess name x t

checkEncodingWithRoundtrip ::
  HasCallStack =>
  Version ->
  (a -> Encoding) ->
  (BSL.ByteString -> Either DecoderError a) ->
  (Version -> (a -> Encoding) -> (BSL.ByteString -> Either DecoderError a) -> a -> Assertion) ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingWithRoundtrip v encode decode roundTrip name x t =
  testCase testName $ do
    unless (expectedBinary == actualBinary) $ do
      expectedTerms <- getTerms "expected" expectedBinary
      actualTerms <- getTerms "actual" actualBinary
      assertFailure $
        unlines
          [ "Serialization did not match: "
          , diffExpr expectedTerms actualTerms
          ]
    roundTrip v encode decode x
  where
    getTerms lbl = either throwIO pure . decodeFullDecoder v lbl decodeTerm
    expectedBinary = serialize v t
    actualBinary = serialize v $ encode x
    testName = "golden_serialize_" <> name

checkEncodingCBORDecodeFailure ::
  (HasCallStack, DecCBOR a, EncCBOR a, Show a) =>
  Version ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBORDecodeFailure v name x t =
  let d = decodeFullDecoder v (fromString name) decCBOR
   in checkEncodingWithRoundtrip v encCBOR d roundTripFailure name x t

checkEncodingCBOR ::
  (HasCallStack, DecCBOR a, EncCBOR a, Show a, Eq a) =>
  Version ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBOR v name x t =
  let d = decodeFullDecoder v (fromString name) decCBOR
   in checkEncodingWithRoundtrip v encCBOR d roundTripSuccess name x t

checkEncodingCBORAnnotated ::
  (HasCallStack, DecCBOR (Annotator a), ToCBOR a, Show a, Eq a) =>
  Version ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBORAnnotated v name x t =
  let d = decodeFullAnnotator v (fromString name) decCBOR
   in checkEncodingWithRoundtrip v (fromPlainEncoding . toCBOR) d roundTripSuccess name x annTokens
  where
    annTokens = T $ TkEncoded $ serialize' v t

data ToTokens where
  T :: (Tokens -> Tokens) -> ToTokens
  S :: EncCBOR a => a -> ToTokens
  G :: EncCBORGroup a => a -> ToTokens
  Plus :: ToTokens -> ToTokens -> ToTokens

instance EncCBOR ToTokens where
  encCBOR (T xs) = fromPlainEncoding (CBOR.Encoding xs)
  encCBOR (S s) = encCBOR s
  encCBOR (G g) = encCBORGroup g
  encCBOR (Plus a b) = encCBOR a <> encCBOR b

instance Semigroup ToTokens where
  (<>) = Plus

instance Monoid ToTokens where
  mempty = T id
