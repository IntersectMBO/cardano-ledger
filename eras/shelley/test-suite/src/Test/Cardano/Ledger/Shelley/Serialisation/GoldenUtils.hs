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
)
where

import Cardano.Ledger.Binary (
  Annotator,
  DecoderError,
  EncCBOR (..),
  Encoding,
  FromCBOR (..),
  ToCBOR (..),
  ToCBORGroup (..),
  Tokens (..),
  Version,
  decodeFullAnnotator,
  decodeFullDecoder,
  decodeTerm,
  fromPlainEncoding,
  serialize,
  serialize',
  serializeEncoding,
  toCBOR,
 )

-- ToExpr (CBOR.Term) instance
import Cardano.Ledger.TreeDiff (diffExpr)
import qualified Codec.CBOR.Encoding as CBOR (Encoding (..))
import Control.Exception (throwIO)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import Data.String (fromString)
import GHC.Stack
import Test.Cardano.Ledger.Binary.TreeDiff ()
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

roundTrip ::
  (HasCallStack, Show a, Eq a) =>
  Version ->
  (a -> Encoding) ->
  (BSL.ByteString -> Either DecoderError a) ->
  a ->
  Assertion
roundTrip v encode decode x =
  case (decode . serializeEncoding v . encode) x of
    Left e -> assertFailure $ "could not decode serialization of " ++ show x ++ ", " ++ show e
    Right y -> y @?= x

checkEncoding ::
  (HasCallStack, Show a, Eq a) =>
  Version ->
  (a -> Encoding) ->
  (BSL.ByteString -> Either DecoderError a) ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncoding v encode decode name x t =
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
    actualBinary = serializeEncoding v $ encode x
    testName = "golden_serialize_" <> name

checkEncodingCBOR ::
  (HasCallStack, FromCBOR a, ToCBOR a, Show a, Eq a) =>
  Version ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBOR v name x t =
  let d = decodeFullDecoder v (fromString name) fromCBOR
   in checkEncoding v toCBOR d name x t

checkEncodingCBORAnnotated ::
  (HasCallStack, FromCBOR (Annotator a), EncCBOR a, Show a, Eq a) =>
  Version ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBORAnnotated v name x t =
  let d = decodeFullAnnotator v (fromString name) fromCBOR
   in checkEncoding v (fromPlainEncoding . encCBOR) d name x annTokens
  where
    annTokens = T $ TkEncoded $ serialize' v t

data ToTokens where
  T :: (Tokens -> Tokens) -> ToTokens
  S :: ToCBOR a => a -> ToTokens
  G :: ToCBORGroup a => a -> ToTokens
  Plus :: ToTokens -> ToTokens -> ToTokens

instance ToCBOR ToTokens where
  toCBOR (T xs) = fromPlainEncoding (CBOR.Encoding xs)
  toCBOR (S s) = toCBOR s
  toCBOR (G g) = toCBORGroup g
  toCBOR (Plus a b) = toCBOR a <> toCBOR b

instance Semigroup ToTokens where
  (<>) = Plus

instance Monoid ToTokens where
  mempty = T id
