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

-- | Defines reusable abstractions for testing RoundTrip properties of CBOR instances
module Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripSpec,
  roundTripRangeSpec,
  roundTripFailureExpectation,
  roundTripExpectation,
  roundTripCborExpectation,
  roundTripAnnExpectation,
  roundTripAnnRangeExpectation,
  roundTripAnnFailureExpectation,
  roundTripAnnFailureRangeExpectation,
  embedTripSpec,
  embedTripExpectation,
  embedTripAnnExpectation,
  roundTripTwiddledProperty,
  roundTripAnnTwiddledProperty,
  RoundTripFailure (..),
  Trip (..),
  mkTrip,
  cborTrip,
  roundTrip,
  roundTripTwiddled,
  roundTripAnn,
  roundTripAnnTwiddled,
  embedTrip,
  embedTripAnn,
  embedTripLabel,
  roundTripRangeExpectation,
  roundTripCborRangeExpectation,
  roundTripFailureCborRangeExpectation,
  roundTripFailureCborExpectation,
)
where

import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import Control.Monad (forM_, guard)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (..), showExpr, showHexBytesGrouped)
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..))
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

-- | Tests the roundtrip property using QuickCheck generators for specific range of versions
roundTripRangeSpec ::
  forall t.
  (Show t, Eq t, Typeable t, Arbitrary t) =>
  Trip t t ->
  Version ->
  Version ->
  Spec
roundTripRangeSpec trip fromVersion toVersion =
  prop (show (typeRep $ Proxy @t)) $ roundTripRangeExpectation trip fromVersion toVersion

-- | Tests the embedtrip property using QuickCheck generators
embedTripSpec ::
  forall a b.
  (Show a, Typeable a, Typeable b, Arbitrary a, HasCallStack) =>
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
  (Show t, Eq t, Typeable t, HasCallStack) =>
  Trip t t ->
  t ->
  Expectation
roundTripExpectation trip = roundTripRangeExpectation trip minBound maxBound

roundTripFailureCborExpectation ::
  forall t.
  (EncCBOR t, DecCBOR t, HasCallStack) =>
  t ->
  Expectation
roundTripFailureCborExpectation = roundTripFailureExpectation (cborTrip @t @t)

roundTripFailureCborRangeExpectation ::
  forall t.
  (EncCBOR t, DecCBOR t, HasCallStack) =>
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  t ->
  Expectation
roundTripFailureCborRangeExpectation = roundTripFailureRangeExpectation (cborTrip @t)

roundTripFailureExpectation ::
  (EncCBOR t, HasCallStack) =>
  Trip t t ->
  t ->
  Expectation
roundTripFailureExpectation trip = roundTripFailureRangeExpectation trip minBound maxBound

roundTripFailureRangeExpectation ::
  forall t.
  (EncCBOR t, HasCallStack) =>
  Trip t t ->
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  t ->
  Expectation
roundTripFailureRangeExpectation trip fromVersion toVersion t =
  forM_ [fromVersion .. toVersion] $ \version ->
    case roundTrip version trip t of
      Left _ -> pure ()
      Right _ -> expectationFailure $ "Should not have deserialized: " <> showExpr (CBORBytes (serialize' version t))

-- | Verify that round triping through the binary form holds for a range of versions.
--
-- In other words check that:
--
-- > deserialize version . serialize version === id
-- > serialize version . deserialize version . serialize version === serialize version
roundTripRangeExpectation ::
  forall t.
  (Show t, Eq t, Typeable t, HasCallStack) =>
  Trip t t ->
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  t ->
  Expectation
roundTripRangeExpectation trip fromVersion toVersion t =
  forM_ [fromVersion .. toVersion] $ \version ->
    case roundTrip version trip t of
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
  (Show t, Eq t, EncCBOR t, DecCBOR t, HasCallStack) =>
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  t ->
  Expectation
roundTripCborRangeExpectation = roundTripRangeExpectation (cborTrip @t)

roundTripAnnExpectation ::
  (Show t, Eq t, ToCBOR t, DecCBOR (Annotator t), HasCallStack) =>
  t ->
  Expectation
roundTripAnnExpectation = roundTripAnnRangeExpectation (natVersion @2) maxBound

roundTripAnnRangeExpectation ::
  (Show t, Eq t, ToCBOR t, DecCBOR (Annotator t), HasCallStack) =>
  Version ->
  Version ->
  t ->
  Expectation
roundTripAnnRangeExpectation fromVersion toVersion t =
  forM_ [fromVersion .. toVersion] $ \version ->
    case roundTripAnn version t of
      Left err -> expectationFailure $ "Failed to deserialize encoded:\n" ++ show err
      Right tDecoded -> tDecoded `shouldBe` t

roundTripAnnFailureExpectation ::
  (ToCBOR t, DecCBOR (Annotator t), HasCallStack) =>
  t ->
  Expectation
roundTripAnnFailureExpectation = roundTripAnnFailureRangeExpectation (natVersion @2) maxBound

roundTripAnnFailureRangeExpectation ::
  (ToCBOR t, DecCBOR (Annotator t), HasCallStack) =>
  Version ->
  Version ->
  t ->
  Expectation
roundTripAnnFailureRangeExpectation fromVersion toVersion t =
  forM_ [fromVersion .. toVersion] $ \version ->
    case roundTripAnn version t of
      Left _ -> pure ()
      Right _ ->
        expectationFailure $
          mconcat
            [ "Should not have deserialized: <version: "
            , show version
            , "> "
            , showExpr (CBORBytes (Plain.serialize' t))
            ]

roundTripTwiddledProperty ::
  (Show t, Eq t, Twiddle t, DecCBOR t) => Version -> t -> Property
roundTripTwiddledProperty version t = property $ do
  roundTripTwiddled version t >>= \case
    Left err ->
      pure $ counterexample ("Failed to deserialize twiddled encoding:\n" ++ show err) False
    Right tDecoded ->
      pure (tDecoded === t)

roundTripAnnTwiddledProperty ::
  forall t q.
  (Twiddle t, DecCBOR (Annotator t), Testable q) =>
  (t -> t -> q) ->
  Version ->
  t ->
  Property
roundTripAnnTwiddledProperty eqProp version t = property $ do
  roundTripAnnTwiddled version t >>= \case
    Left err ->
      pure $ counterexample ("Failed to deserialize twiddled encoding:\n" ++ show err) False
    Right tDecoded ->
      pure $ property (tDecoded `eqProp` t)

embedTripExpectation ::
  forall a b.
  (Typeable b, HasCallStack) =>
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
  , rtfReEncodedBytes :: Maybe (BSL.ByteString)
  -- ^ Re-serialized bytes, if there was a mismatch between the binary form and the
  -- reserialization of the data type.
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
      , prettyTerm
      ]
        ++ showHexBytesGrouped bytes
    where
      showMaybeDecoderError name = \case
        Nothing -> "No " ++ name ++ " error"
        Just err -> name ++ " error: " ++ showDecoderError err
      bytes = BSL.toStrict rtfEncodedBytes
      prettyTerm =
        case decodeFullDecoder' rtfDecoderVersion "Term" decodeTerm bytes of
          Left err -> "Could not decode as Term: " ++ show err
          Right term -> showExpr term

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

roundTrip :: forall t. Typeable t => Version -> Trip t t -> t -> Either RoundTripFailure t
roundTrip version trip val = do
  (val', encoding, encodedBytes) <- embedTripLabelExtra (typeLabel @t) version version trip val
  let reserialized = serialize version (tripEncoder trip val')
  if reserialized /= encodedBytes
    then
      Left $
        RoundTripFailure version version encoding encodedBytes (Just reserialized) Nothing Nothing
    else Right val'

roundTripTwiddled ::
  forall t.
  (Twiddle t, DecCBOR t) =>
  Version ->
  t ->
  Gen (Either RoundTripFailure t)
roundTripTwiddled version x = do
  tw <- twiddle version x
  pure (roundTrip version (Trip (const (encodeTerm tw)) decCBOR (dropCBOR (Proxy @t))) x)

roundTripAnn :: (ToCBOR t, DecCBOR (Annotator t)) => Version -> t -> Either RoundTripFailure t
roundTripAnn v = embedTripAnn v v

roundTripAnnTwiddled ::
  (Twiddle t, DecCBOR (Annotator t)) => Version -> t -> Gen (Either RoundTripFailure t)
roundTripAnnTwiddled version x = do
  tw <- twiddle version x
  pure (decodeAnn version version (toPlainEncoding version (encodeTerm tw)))

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
  first (RoundTripFailure encVersion decVersion encoding encodedBytes Nothing Nothing . Just) $
    decodeFullAnnotator decVersion (label (Proxy @(Annotator t))) decCBOR encodedBytes
  where
    encodedBytes = Plain.serialize encoding

embedTripLabel ::
  forall a b.
  T.Text ->
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTripLabel lbl encVersion decVersion trip s =
  (\(val, _, _) -> val) <$> embedTripLabelExtra lbl encVersion decVersion trip s

embedTripLabelExtra ::
  forall a b.
  T.Text ->
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure (b, Plain.Encoding, BSL.ByteString)
embedTripLabelExtra lbl encVersion decVersion (Trip encoder decoder dropper) s =
  case decodeFullDecoder decVersion lbl decoder encodedBytes of
    Right val
      | Nothing <- mDropperError -> Right (val, encoding, encodedBytes)
      | Just err <- mDropperError ->
          Left $
            RoundTripFailure encVersion decVersion encoding encodedBytes Nothing (Just err) Nothing
    Left err ->
      let mErr = do
            dropperError <- mDropperError
            guard (dropperError /= err)
            pure dropperError
       in Left $
            RoundTripFailure encVersion decVersion encoding encodedBytes Nothing mErr (Just err)
  where
    encoding = toPlainEncoding encVersion (encoder s)
    encodedBytes = Plain.serialize encoding
    mDropperError =
      case decodeFullDecoder decVersion lbl dropper encodedBytes of
        Left err -> Just err
        Right () -> Nothing

-- | Can we serialise a type, and then deserialise it as something else?
embedTrip ::
  forall a b.
  Typeable b =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTrip = embedTripLabel (T.pack (show (typeRep $ Proxy @b)))

embedTripAnn ::
  forall a b.
  (ToCBOR a, DecCBOR (Annotator b)) =>
  -- | Encoder version for test failure reporting
  Version ->
  -- | Decoder version
  Version ->
  a ->
  Either RoundTripFailure b
embedTripAnn encVersion decVersion = decodeAnn encVersion decVersion . toCBOR

typeLabel :: forall t. Typeable t => T.Text
typeLabel = T.pack (show (typeRep $ Proxy @t))
