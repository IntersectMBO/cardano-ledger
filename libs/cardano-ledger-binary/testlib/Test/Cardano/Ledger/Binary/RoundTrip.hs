{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines reusable abstractions for testing RoundTrip properties of CBOR instances
module Test.Cardano.Ledger.Binary.RoundTrip (
  -- * Spec
  roundTripSpec,
  roundTripCborSpec,
  roundTripRangeSpec,

  -- * Expectations

  -- ** Trip
  roundTripExpectation,
  roundTripRangeExpectation,
  roundTripFailureExpectation,
  roundTripRangeFailureExpectation,

  -- ** Enc/DecCBOR
  roundTripCborExpectation,
  roundTripCborRangeExpectation,
  roundTripCborFailureExpectation,
  roundTripCborRangeFailureExpectation,

  -- ** Embed
  embedTripSpec,
  embedTripExpectation,
  embedTripFailureExpectation,
  embedTripRangeFailureExpectation,
  roundTripTwiddledProperty,

  -- * Tripping failure
  RoundTripFailure (..),

  -- * Tripping definitions
  Trip (..),
  mkTrip,
  cborTrip,

  -- * Tripping functions
  roundTrip,
  roundTripTwiddled,
  embedTrip,
  embedTripLabel,
  embedTripLabelExtra,

  -- * Annotator evidence
  AnnotatorEvidence (..),
) where

import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Codec.CBOR.FlatTerm as CBOR
import Control.Monad (forM_, guard)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import Test.Cardano.Ledger.Binary.Plain.RoundTrip (
  showFailedTermsWithReSerialization,
  showMaybeDecoderError,
 )
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (..), ToExpr, ansiExprString)
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (label)

-- =====================================================================

-- | The `IsAnnotated a ~` constraint ensures that `EvidenceAnnotated` can only
-- be constructed if `a` is an `Annotator` of some type and vice versa for
-- `EvidencePlain`
data AnnotatorEvidence a
  = DecCBOR (Annotator a) => EvidenceAnnotated
  | (Typeable a, Show a, Eq a, Ord a, ToExpr a, DecCBOR a, ToCBOR a) => EvidencePlain

-- | Tests the roundtrip property using QuickCheck generators for all possible versions
-- starting with `shelleyProtVer`.
roundTripSpec ::
  forall t.
  (Show t, Eq t, Arbitrary t, Typeable t) =>
  Trip t t ->
  Spec
roundTripSpec trip =
  prop (show (typeRep $ Proxy @t)) $ roundTripExpectation trip

-- | Tests the roundtrip property using QuickCheck generators for all possible versions
-- starting with `shelleyProtVer`.
roundTripCborSpec ::
  forall t.
  (Show t, Eq t, Arbitrary t, EncCBOR t, DecCBOR t) =>
  Spec
roundTripCborSpec = roundTripSpec (cborTrip @t)

-- | Tests the roundtrip property using QuickCheck generators for specific range of versions
roundTripRangeSpec ::
  forall t.
  (Show t, Eq t, Typeable t, Arbitrary t) =>
  Trip t t ->
  Version ->
  Version ->
  Spec
roundTripRangeSpec trip fromVersion toVersion =
  prop (show (typeRep $ Proxy @t)) $ roundTripRangeExpectation undefined trip fromVersion toVersion

-- | Tests the embedtrip property using QuickCheck generators
embedTripSpec ::
  forall a b.
  (Show a, Typeable a, Typeable b, Arbitrary a, Eq b, HasCallStack) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  (b -> a -> Expectation) ->
  Spec
embedTripSpec encVersion decVersion trip f =
  prop ("From: " ++ show (typeRep $ Proxy @a) ++ " To " ++ show (typeRep $ Proxy @b)) $
    embedTripExpectation encVersion decVersion trip f

-- | Verify that round triping through the binary form holds for all versions starting
-- with `shelleyProtVer`.
roundTripExpectation ::
  (Show t, Eq t, HasCallStack) =>
  Trip t t ->
  t ->
  Expectation
roundTripExpectation trip = roundTripRangeExpectation undefined trip minBound maxBound

roundTripCborFailureExpectation ::
  forall t.
  (EncCBOR t, DecCBOR t, Eq t, HasCallStack) =>
  t ->
  Expectation
roundTripCborFailureExpectation = roundTripFailureExpectation (cborTrip @t @t)

roundTripCborRangeFailureExpectation ::
  forall t.
  (EncCBOR t, DecCBOR t, Eq t, HasCallStack) =>
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  t ->
  Expectation
roundTripCborRangeFailureExpectation = roundTripRangeFailureExpectation (cborTrip @t)

roundTripFailureExpectation ::
  (Typeable t, Eq t, HasCallStack) =>
  Trip t t ->
  t ->
  Expectation
roundTripFailureExpectation trip = roundTripRangeFailureExpectation trip minBound maxBound

roundTripRangeFailureExpectation ::
  forall t.
  (Typeable t, Eq t, HasCallStack) =>
  Trip t t ->
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  t ->
  Expectation
roundTripRangeFailureExpectation = embedTripRangeFailureExpectation

embedTripFailureExpectation ::
  (Typeable b, Eq b, HasCallStack) =>
  Trip a b ->
  a ->
  Expectation
embedTripFailureExpectation trip = embedTripRangeFailureExpectation trip minBound maxBound

embedTripRangeFailureExpectation ::
  forall a b.
  (Typeable b, Eq b, HasCallStack) =>
  Trip a b ->
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  a ->
  Expectation
embedTripRangeFailureExpectation trip fromVersion toVersion t =
  forM_ [fromVersion .. toVersion] $ \version ->
    case embedTripLabelExtra (typeLabel @b) version version trip t of
      Left _ -> pure ()
      Right (_, _, bs) ->
        expectationFailure $
          mconcat
            [ "Should not have deserialized: <version: "
            , show version
            , "> "
            , ansiExprString (CBORBytes (BSL.toStrict bs))
            ]

-- | Verify that round triping through the binary form holds for a range of versions.
--
-- In other words check that:
--
-- > deserialize version . serialize version === id
-- > serialize version . deserialize version . serialize version === serialize version
roundTripRangeExpectation ::
  forall t.
  (Show t, Eq t, HasCallStack) =>
  AnnotatorEvidence t ->
  Trip t t ->
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  t ->
  Expectation
roundTripRangeExpectation annEv trip fromVersion toVersion t =
  forM_ [fromVersion .. toVersion] $ \version ->
    let
      tripRes =
        case annEv of
          EvidencePlain -> roundTrip version trip t
          EvidenceAnnotated -> undefined
     in
      case tripRes of
        Left err -> expectationFailure $ "Failed to deserialize encoded:\n" ++ show err
        Right tDecoded -> tDecoded `shouldBe` t

roundTripCborExpectation ::
  forall t.
  (Show t, Eq t, EncCBOR t, DecCBOR t, HasCallStack) =>
  t ->
  Expectation
roundTripCborExpectation = roundTripExpectation (cborTrip @t @t)

roundTripCborRangeExpectation ::
  forall t.
  (EncCBOR t, HasCallStack, Show t, DecCBOR t, Ord t) =>
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  t ->
  Expectation
roundTripCborRangeExpectation = roundTripRangeExpectation undefined (cborTrip @t)

roundTripTwiddledProperty ::
  (Show t, Eq t, Twiddle t, ToCBOR t) => Version -> t -> Property
roundTripTwiddledProperty version t = property $ do
  roundTripTwiddled undefined version t >>= \case
    Left err ->
      pure $ counterexample ("Failed to deserialize twiddled encoding:\n" ++ show err) False
    Right tDecoded ->
      pure (tDecoded === t)

embedTripExpectation ::
  forall a b.
  (Typeable b, Eq b, HasCallStack) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  (b -> a -> Expectation) ->
  a ->
  Expectation
embedTripExpectation encVersion decVersion trip f t =
  case embedTrip encVersion decVersion trip t of
    Left err -> expectationFailure $ "Failed to deserialize encoded:\n" ++ show err
    Right tDecoded -> f tDecoded t

-- | This is just like `roundTripAnnExpectation`, except it allows for source and target
-- types to be different. This is very useful to test translation of the same type family
-- from one era to another.
embedTripAnnExpectation ::
  forall a b.
  (ToCBOR a, DecCBOR (Annotator b), HasCallStack) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  (b -> a -> Expectation) ->
  a ->
  Expectation
embedTripAnnExpectation encVersion decVersion f a =
  case embedTripAnn encVersion decVersion a of
    Left err -> expectationFailure $ "Failed to deserialize encoded:\n" ++ show err
    Right b -> b `f` a

-- =====================================================================

data RoundTripFailure = RoundTripFailure
  { rtfEncoderVersion :: Version
  -- ^ Version that was used during encoding
  , rtfDecoderVersion :: Version
  -- ^ Version that was used during decoding
  , rtfEncoding :: Plain.Encoding
  -- ^ Produced plain encoding
  , rtfEncodedBytes :: BSL.ByteString
  -- ^ Serialized encoding using the version in this failure
  , rtfReEncodedBytes :: Maybe BSL.ByteString
  -- ^ Re-serialized bytes, if there was a mismatch between the binary form and the
  -- reserialization of the data type.
  , rtfConformanceError :: Maybe String
  -- ^ Roundtripping through FlatTerm
  , rtfDropperError :: Maybe DecoderError
  -- ^ Error received while decoding the produced bytes and dropping the value. Normally
  -- it will be `Nothing`, unless the error produced did not match the
  -- `rtfDecoderError`, in which case it will be `Just` the error.
  , rtfDecoderError :: Maybe DecoderError
  -- ^ Error received while decoding the produced bytes. It is possible for a dropper to
  -- produce an error, while decoder going through successfully, which constitues a test
  -- failure. In such a case this field will be `Nothing`, however `rtfDropperError`
  -- will be set to `Just`. Whenever both `rtfDropperError` and `rtfDecoderError` are
  -- `Nothing` it means that the decoding went though just fine, but there was a
  -- mismatch in the binary format, i.e. reserialization produced a mismatched result,
  -- in which case `rtfReEncodedBytes` will be set to `Just`
  }

instance Show RoundTripFailure where
  show RoundTripFailure {..} =
    unlines $
      [ "Encoder Version: " ++ show rtfEncoderVersion
      , "Decoder Version: " ++ show rtfDecoderVersion
      , showMaybeDecoderError "Decoder" rtfDecoderError
      , showMaybeDecoderError "Dropper" rtfDropperError
      , showMaybeDecoderError "Conformance" rtfConformanceError
      ]
        ++ [ "Original did not match the reserialization (see below)."
           | Just _ <- pure rtfReEncodedBytes
           ]
        ++ showFailedTermsWithReSerialization rtfEncodedBytes rtfReEncodedBytes

-- | A definition of a CBOR trip through binary representation of one type to
-- another. In this module this is called an embed. When a source and target type is the
-- exact same one then it would be a dual and is expected to round trip.
data Trip a b = Trip
  { tripEncoder :: a -> Encoding
  , tripDecoder :: forall s. Decoder s b
  , tripDropper :: forall s. Decoder s ()
  }

cborTrip :: forall a b. (EncCBOR a, DecCBOR b) => Trip a b
cborTrip = Trip encCBOR decCBOR (dropCBOR (Proxy @b))

-- | Construct a `Trip` using encoder and decoder, with dropper set to the decoder which
-- drops the value
mkTrip :: forall a b. (a -> Encoding) -> (forall s. Decoder s b) -> Trip a b
mkTrip encoder decoder = Trip encoder decoder (() <$ decoder)

-- | Check that serialization followed by deserialization of the value produces the same
-- value back. We also check that re-serialization is idempotent. In other words, we
-- ensure that deserialization does not modify the decoded value in a way that its binary
-- representation has changed. Dropper is checked as well.
roundTrip ::
  forall t.
  (ToCBOR t, Eq t) =>
  Version ->
  Trip t t ->
  t ->
  Either RoundTripFailure t
roundTrip version trip val = do
  (val', encoding, encodedBytes) <- embedTripLabelExtra (typeLabel @t) version version trip val
  let reserialized = serialize version (tripEncoder trip val')
  if reserialized /= encodedBytes
    then
      Left $
        RoundTripFailure version version encoding encodedBytes (Just reserialized) Nothing Nothing Nothing
    else Right val'

roundTripTwiddled ::
  forall t.
  (Twiddle t, ToCBOR t) =>
  AnnotatorEvidence t ->
  Version ->
  t ->
  Gen (Either RoundTripFailure t)
roundTripTwiddled annEv version x = do
  tw <- twiddle version x
  pure $ case annEv of
    EvidencePlain -> roundTrip version (Trip (const (encodeTerm tw)) decCBOR (dropCBOR (Proxy @t))) x
    EvidenceAnnotated -> decodeAnn version version . toPlainEncoding version $ encodeTerm tw

decodeAnn ::
  forall t.
  DecCBOR (Annotator t) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Plain.Encoding ->
  Either RoundTripFailure t
decodeAnn encVersion decVersion encoding =
  fst <$> decodeAnnExtra (label (Proxy @(Annotator t))) encVersion decVersion decCBOR encoding

decodeAnnExtra ::
  forall t.
  T.Text ->
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  (forall s. Decoder s (Annotator t)) ->
  Plain.Encoding ->
  Either RoundTripFailure (t, BSL.ByteString)
decodeAnnExtra lbl encVersion decVersion decoder encoding
  | CBOR.validFlatTerm flatTerm =
      bimap (mkRoundTripFailure Nothing Nothing Nothing . Just) (,encodedBytes) $
        decodeFullAnnotator decVersion lbl decoder encodedBytes
  | otherwise =
      Left $ mkRoundTripFailure (Just "FlatTerm encoding is invalid") Nothing Nothing Nothing
  where
    mkRoundTripFailure = RoundTripFailure encVersion decVersion encoding encodedBytes
    encodedBytes = Plain.serialize encoding
    flatTerm = CBOR.toFlatTerm encoding

embedTripLabel ::
  forall a b.
  Eq b =>
  T.Text ->
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTripLabel lbl encVersion decVersion trip s =
  embedTripLabelExtra lbl encVersion decVersion trip s <&> \case
    (res, _, _) -> res

embedTripLabelExtra ::
  forall a b.
  Eq b =>
  T.Text ->
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure (b, Plain.Encoding, BSL.ByteString)
embedTripLabelExtra lbl encVersion decVersion (Trip encoder decoder dropper) s = result
  where
    mkFailure = RoundTripFailure encVersion decVersion encoding encodedBytes Nothing
    result =
      case decodeFullDecoder decVersion lbl decoder encodedBytes of
        Right val
          | Nothing <- mDropperError ->
              let flatTerm = CBOR.toFlatTerm encoding
               in -- We must not pass original bytes, because FlatTerm can't handle offsets
                  case CBOR.fromFlatTerm (toPlainDecoder Nothing decVersion decoder) flatTerm of
                    Left err
                      | err == originalBytesExpectedFailureMessage -> Right (val, encoding, encodedBytes)
                      | otherwise ->
                          Left $ mkFailure (Just $ "fromFlatTerm error:" <> err) Nothing Nothing
                    Right valFromFlatTerm
                      | val /= valFromFlatTerm ->
                          let errMsg =
                                "Deserializing through FlatTerm produced a different "
                                  ++ "value then the regular deserializer did"
                           in Left $ mkFailure (Just errMsg) Nothing Nothing
                      | not (CBOR.validFlatTerm flatTerm) ->
                          let errMsg =
                                "Despite successful deserialization the produced "
                                  ++ "FlatTerm for the type is not valid"
                           in Left $ mkFailure (Just errMsg) Nothing Nothing
                      | otherwise -> Right (val, encoding, encodedBytes)
          | Just err <- mDropperError -> Left $ mkFailure Nothing (Just err) Nothing
        Left err ->
          -- In case of failure we only record dropper error iff it differs from the
          -- decoder failure:
          let mErr = do
                dropperError <- mDropperError
                dropperError <$ guard (dropperError /= err)
           in Left $ mkFailure Nothing mErr (Just err)
    encoding = toPlainEncoding encVersion (encoder s)
    encodedBytes = Plain.serialize encoding
    mDropperError =
      case decodeFullDecoder decVersion lbl dropper encodedBytes of
        Left err -> Just err
        Right () -> Nothing

-- | Can we serialise a type, and then deserialise it as something else?
embedTrip ::
  forall a b.
  (Eq b, Typeable b) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTrip = embedTripLabel (T.pack (show (typeRep $ Proxy @b)))

typeLabel :: forall t. Typeable t => T.Text
typeLabel = T.pack (show (typeRep $ Proxy @t))
