{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines reusable abstractions for testing RoundTrip properties of plain encoders/decoders
module Test.Cardano.Ledger.Binary.Plain.RoundTrip (
  roundTripSpec,
  roundTripFailureExpectation,
  roundTripExpectation,
  roundTripCborExpectation,
  embedTripSpec,
  embedTripExpectation,
  RoundTripFailure (..),
  showMaybeDecoderError,
  showFailedTermsWithReSerialization,
  Trip (..),
  mkTrip,
  cborTrip,
  roundTrip,
  embedTrip,
  embedTripLabel,
)
where

import Cardano.Ledger.Binary.Plain
import qualified Codec.CBOR.FlatTerm as CBOR
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as Text
import Data.Typeable
import qualified Formatting.Buildable as B (Buildable (..))
import Test.Cardano.Ledger.Binary.TreeDiff (
  CBORBytes (..),
  ansiExprString,
  diffExprString,
  showHexBytesGrouped,
 )
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (label)

-- =====================================================================

-- | Tests the roundtrip property using QuickCheck generators for all possible versions
-- starting with `shelleyProtVer`.
roundTripSpec ::
  forall t.
  (Show t, Eq t, Typeable t, Arbitrary t) =>
  Trip t t ->
  Spec
roundTripSpec trip =
  prop (show (typeRep $ Proxy @t)) $ roundTripExpectation trip

-- | Tests the embedtrip property using QuickCheck generators
embedTripSpec ::
  forall a b.
  (Eq b, Show a, Typeable a, Typeable b, Arbitrary a, HasCallStack) =>
  Trip a b ->
  (b -> a -> Expectation) ->
  Spec
embedTripSpec trip f =
  prop ("From: " ++ show (typeRep $ Proxy @a) ++ " To " ++ show (typeRep $ Proxy @b)) $
    embedTripExpectation trip f

-- Tests that a decoder error happens
roundTripFailureExpectation :: forall a. (Eq a, ToCBOR a, FromCBOR a) => a -> Expectation
roundTripFailureExpectation x =
  case roundTrip (cborTrip @a) x of
    Left _ -> pure ()
    Right _ ->
      expectationFailure $
        "Should not have deserialized: "
          <> ansiExprString (CBORBytes (serialize' x))

-- | Verify that round triping through the binary form holds
roundTripExpectation ::
  (Show t, Eq t, Typeable t, HasCallStack) =>
  Trip t t ->
  t ->
  Expectation
roundTripExpectation trip t =
  case roundTrip trip t of
    Left err -> expectationFailure $ "Failed to deserialize encoded:\n" ++ show err
    Right tDecoded -> tDecoded `shouldBe` t

roundTripCborExpectation ::
  forall t.
  (Show t, Eq t, FromCBOR t, ToCBOR t, HasCallStack) =>
  t ->
  Expectation
roundTripCborExpectation = roundTripExpectation (cborTrip @t)

embedTripExpectation ::
  forall a b.
  (Typeable b, Eq b, HasCallStack) =>
  Trip a b ->
  (b -> a -> Expectation) ->
  a ->
  Expectation
embedTripExpectation trip f t =
  case embedTrip trip t of
    Left err -> expectationFailure $ "Failed to deserialize encoded:\n" ++ show err
    Right tDecoded -> f tDecoded t

-- =====================================================================

data RoundTripFailure = RoundTripFailure
  { rtfEncoding :: Encoding
  -- ^ Produced plain encoding
  , rtfEncodedBytes :: BSL.ByteString
  -- ^ Serialized encoding
  , rtfReEncodedBytes :: Maybe BSL.ByteString
  -- ^ Re-serialized bytes, if there was a mismatch between the binary form and the
  -- reserialization of the data type.
  , rtfFlatTermError :: Maybe String
  -- ^ Roundtripping through FlatTerm
  , rtfDecoderError :: Maybe DecoderError
  -- ^ Error received while decoding the produced bytes.
  }

instance Show RoundTripFailure where
  show RoundTripFailure {..} =
    unlines $
      [ showMaybeDecoderError "Decoder" rtfDecoderError
      , showMaybeDecoderError "FlatTerm" rtfFlatTermError
      ]
        ++ [ "Original did not match the reserialization (see below)."
           | Just _ <- pure rtfReEncodedBytes
           ]
        ++ showFailedTermsWithReSerialization rtfEncodedBytes rtfReEncodedBytes

showMaybeDecoderError :: B.Buildable b => String -> Maybe b -> String
showMaybeDecoderError name = \case
  Nothing -> "No " ++ name ++ " error"
  Just err -> name ++ " error: " ++ showDecoderError err

showFailedTermsWithReSerialization ::
  BSL.ByteString ->
  Maybe BSL.ByteString ->
  [String]
showFailedTermsWithReSerialization encodedBytes mReEncodedBytes =
  case mReEncodedBytes of
    Nothing ->
      -- This is the usual case where re-serialization is successful
      let (_, origHex, origStr) = termWithHex "Original" encodedBytes
       in origStr ++ ["OriginalHex:"] ++ origHex
    Just reBytes ->
      -- On a rare occasion when re-serialization does not match we try to show the
      -- diff of Hex as well as Terms
      case (termWithHex "Original" encodedBytes, termWithHex "Reserialization" reBytes) of
        ((Right origTerm, origHex, _), (Right reTerm, reHex, _)) ->
          [diffExprString (origTerm, origHex) (reTerm, reHex)]
        ((_, origHex, origStr), (_, reHex, reStr)) ->
          diffExprString origHex reHex : origStr <> reStr
  where
    termWithHex name lazyBytes =
      let bytes = BSL.toStrict lazyBytes
          decTerm = case decodeFullDecoder' "Term" decodeTerm bytes of
            Left err -> Left $ "Could not decode as Term: " <> show err
            Right term -> Right term
          hexLines = showHexBytesGrouped 128 bytes
       in (decTerm, hexLines, [name <> ":", either id ansiExprString decTerm])

-- | A definition of a CBOR trip through binary representation of one type to
-- another. In this module this is called an embed. When a source and target type is the
-- exact same one then it would be a dual and is expected to round trip.
data Trip a b = Trip
  { tripEncoder :: a -> Encoding
  , tripDecoder :: forall s. Decoder s b
  }

cborTrip :: forall a b. (ToCBOR a, FromCBOR b) => Trip a b
cborTrip = Trip toCBOR fromCBOR

-- | Construct a `Trip` using encoder and decoder, with dropper set to the decoder which
-- drops the value
mkTrip :: forall a b. (a -> Encoding) -> (forall s. Decoder s b) -> Trip a b
mkTrip = Trip

roundTrip :: forall t. (Eq t, Typeable t) => Trip t t -> t -> Either RoundTripFailure t
roundTrip trip val = do
  (val', encoding, encodedBytes) <- embedTripLabelExtra (typeLabel @t) trip val
  let reserialized = serialize (tripEncoder trip val')
  if reserialized /= encodedBytes
    then
      Left $
        RoundTripFailure encoding encodedBytes (Just reserialized) Nothing Nothing
    else Right val'

embedTripLabel ::
  forall a b.
  Eq b =>
  Text.Text ->
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTripLabel lbl trip s =
  (\(val, _, _) -> val) <$> embedTripLabelExtra lbl trip s

embedTripLabelExtra ::
  forall a b.
  Eq b =>
  Text.Text ->
  Trip a b ->
  a ->
  Either RoundTripFailure (b, Encoding, BSL.ByteString)
embedTripLabelExtra lbl (Trip encoder decoder) s =
  case decodeFullDecoder lbl decoder encodedBytes of
    Right val ->
      let flatTerm = CBOR.toFlatTerm encoding
       in case CBOR.fromFlatTerm decoder flatTerm of
            Left err -> Left $ mkFailure Nothing (Just err) Nothing
            Right valFromFlatTerm
              | val /= valFromFlatTerm ->
                  let errMsg =
                        "Deserializoing through FlatTerm produced a different "
                          ++ "value then the regular deserializer did"
                   in Left $ mkFailure Nothing (Just errMsg) Nothing
              | not (CBOR.validFlatTerm flatTerm) ->
                  let errMsg =
                        "Despite successful deserialization the produced "
                          ++ "FlatTerm for the type is not valid"
                   in Left $ mkFailure Nothing (Just errMsg) Nothing
              | otherwise -> Right (val, encoding, encodedBytes)
    Left err ->
      Left $ mkFailure Nothing Nothing (Just err)
  where
    mkFailure = RoundTripFailure encoding encodedBytes
    encoding = encoder s
    encodedBytes = serialize encoding

-- | Can we serialise a type, and then deserialise it as something else?
embedTrip ::
  forall a b.
  (Eq b, Typeable b) =>
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTrip = embedTripLabel (Text.pack (show (typeRep $ Proxy @b)))

typeLabel :: forall t. Typeable t => Text.Text
typeLabel = Text.pack (show (typeRep $ Proxy @t))
