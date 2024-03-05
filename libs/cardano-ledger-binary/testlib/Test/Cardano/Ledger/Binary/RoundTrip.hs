{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines reusable abstractions for testing RoundTrip properties of CBOR instances
module Test.Cardano.Ledger.Binary.RoundTrip (
  -- * Spec
  roundTripSpec,
  roundTripSpecIO,
  roundTripCborSpec,
  roundTripAnnCborSpec,
  roundTripRangeSpec,

  -- * Expectations

  -- ** Trip
  roundTripExpectation,
  roundTripExpectationIO,
  roundTripRangeExpectation,
  roundTripRangeExpectationIO,
  roundTripFailureExpectation,
  roundTripRangeFailureExpectation,

  -- ** Enc/DecCBOR
  roundTripCborExpectation,
  roundTripCborRangeExpectation,
  roundTripCborFailureExpectation,
  roundTripCborRangeFailureExpectation,
  roundTripAnnExpectation,
  roundTripAnnRangeExpectation,
  roundTripAnnFailureExpectation,
  roundTripAnnRangeFailureExpectation,

  -- ** Embed
  embedTripSpec,
  embedTripExpectation,
  embedTripAnnExpectation,
  embedTripFailureExpectation,
  embedTripRangeFailureExpectation,
  roundTripTwiddledProperty,
  roundTripAnnTwiddledProperty,

  -- * Tripping failure
  RoundTripFailure (..),

  -- * Tripping definitions
  Trip,
  TripIO,
  TripOf (..),
  pureTrip,
  mkTrip,
  cborTrip,
  cborTripIO,

  -- * Tripping functions
  roundTrip,
  roundTripIO,
  roundTripTwiddled,
  roundTripAnn,
  roundTripAnnTwiddled,
  embedTrip,
  embedTripAnn,
  embedTripLabel,
  embedTripLabelExtra,
  decodeAnnExtra,

)
where

import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Codec.CBOR.FlatTerm as CBOR
-- import Control.Monad (forM_, guard)
import Control.Monad.Identity
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
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (..), showExpr)
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (label)

-- =====================================================================

-- | Tests the roundtrip property using pure QuickCheck generators for all
-- possible versions starting with `shelleyProtVer`.
roundTripSpec ::
  forall t.
  (Show t, Eq t, Typeable t, Arbitrary t) =>
  Trip t t ->
  Spec
roundTripSpec trip =
  prop (show (typeRep $ Proxy @t)) $ roundTripExpectation trip

-- | Tests the roundtrip property using QuickCheck generators in IO for all
-- possible versions starting with `shelleyProtVer`. See 'roundTripSpec'.
roundTripSpecIO ::
  forall t s.
  (Show s, Show t, Eq t, Typeable t, Arbitrary s) =>
  TripIO s t t ->
  Spec
roundTripSpecIO trip =
  prop (show (typeRep $ Proxy @t)) $ roundTripExpectationIO trip


-- | Tests the roundtrip property using QuickCheck generators for all possible versions
-- starting with `shelleyProtVer`.
roundTripCborSpec ::
  forall t.
  (Show t, Eq t, Arbitrary t, EncCBOR t, DecCBOR t) =>
  Spec
roundTripCborSpec = roundTripSpec (cborTrip @t)

-- | Tests the roundtrip property using QuickCheck generators for all possible versions
-- starting with `shelleyProtVer`.
roundTripAnnCborSpec ::
  forall t.
  (Show t, Eq t, Arbitrary t, ToCBOR t, DecCBOR (Annotator t)) =>
  Spec
roundTripAnnCborSpec = prop (show (typeRep $ Proxy @t)) (roundTripAnnExpectation @t)

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
  (Show t, Eq t, Typeable t, HasCallStack) =>
  Trip t t ->
  t ->
  Expectation
roundTripExpectation trip = roundTripRangeExpectation trip minBound maxBound

-- | Verify that round triping through the binary form holds for all versions starting
-- with `shelleyProtVer`.
roundTripExpectationIO ::
  (Show t, Eq t, Typeable t, HasCallStack) =>
  TripIO s t t ->
  s ->
  Expectation
roundTripExpectationIO trip = roundTripRangeExpectationIO trip minBound maxBound

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
            , showExpr (CBORBytes (BSL.toStrict bs))
            ]

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


-- | Verify that round triping through the binary form holds for a range of versions.
--
-- In other words check that:
--
-- > deserialize version . serialize version === id
-- > serialize version . deserialize version . serialize version === serialize version
roundTripRangeExpectationIO ::
  forall t s.
  (Show t, Eq t, Typeable t, HasCallStack) =>
  TripIO s t t ->
  -- | From Version
  Version ->
  -- | To Version
  Version ->
  s ->
  Expectation
roundTripRangeExpectationIO trip fromVersion toVersion s = do
  forM_ [fromVersion .. toVersion] $ \version -> do
    t <- tripGenerator trip s
    roundTripIO version trip s >>= \case
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
roundTripAnnFailureExpectation = roundTripAnnRangeFailureExpectation (natVersion @2) maxBound

roundTripAnnRangeFailureExpectation ::
  (ToCBOR t, DecCBOR (Annotator t), HasCallStack) =>
  Version ->
  Version ->
  t ->
  Expectation
roundTripAnnRangeFailureExpectation fromVersion toVersion t =
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
data TripOf m s a b = Trip
  { tripGenerator :: s -> m a
  , tripEncoder :: a -> Encoding
  , tripDecoder :: forall e. Decoder e b
  , tripDropper :: forall e. Decoder e ()
  }

type Trip a b = TripOf Identity a a b

type TripIO s a b = TripOf IO s a b

pureTrip :: (a -> Encoding) -> (forall s. Decoder s b) -> (forall s. Decoder s ()) -> Trip a b
pureTrip enc dec decDrop =
  Trip pure enc dec decDrop

cborTrip :: forall a b. (EncCBOR a, DecCBOR b) => Trip a b
cborTrip = pureTrip encCBOR decCBOR (dropCBOR (Proxy @b))

cborTripIO :: forall s a b. (EncCBOR a, DecCBOR b) => (s -> IO a) -> TripIO s a b
cborTripIO generator = Trip generator encCBOR decCBOR (dropCBOR (Proxy @b))

-- | Construct a `Trip` using encoder and decoder, with dropper set to the decoder which
-- drops the value
mkTrip :: forall a b. (a -> Encoding) -> (forall s. Decoder s b) -> Trip a b
mkTrip encoder decoder = Trip pure encoder decoder (() <$ decoder)

-- -- | Construct a `Trip` using encoder and decoder, with dropper set to the decoder which
-- -- drops the value
-- mkTripIO :: forall s a b. (s -> IO a) -> (a -> Encoding) -> (forall e. Decoder e b) -> TripIO s a b
-- mkTripIO generator encoder decoder = Trip generator encoder decoder (() <$ decoder)

-- | Check that serialization followed by deserialization of the value produces the same
-- value back. We also check that re-serialization is idempotent. In other words, we
-- ensure that deserialization does not modify the decoded value in a way that its binary
-- representation has changed. Dropper is checked as well.
roundTrip ::
  forall t.
  (Eq t, Typeable t) =>
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

-- | Check that serialization followed by deserialization of the value produces the same
-- value back. We also check that re-serialization is idempotent. In other words, we
-- ensure that deserialization does not modify the decoded value in a way that its binary
-- representation has changed. Dropper is checked as well.
roundTripIO ::
  forall t s.
  (Eq t, Typeable t) =>
  Version ->
  TripIO s t t ->
  s ->
  IO (Either RoundTripFailure t)
roundTripIO version trip seed = do
  val <- tripGenerator trip seed
  return $ do
    (val', encoding, encodedBytes) <- embedTripLabelExtra (typeLabel @t) version version trip val
    let reserialized = serialize version (tripEncoder trip val')
    if reserialized /= encodedBytes
      then
        Left $
          RoundTripFailure version version encoding encodedBytes (Just reserialized) Nothing Nothing Nothing
      else Right val'

roundTripTwiddled ::
  forall t.
  (Twiddle t, DecCBOR t, Eq t) =>
  Version ->
  t ->
  Gen (Either RoundTripFailure t)
roundTripTwiddled version x = do
  tw <- twiddle version x
  pure (roundTrip version (pureTrip (const (encodeTerm tw)) decCBOR (dropCBOR (Proxy @t))) x)

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
  forall m s a b.
  Eq b =>
  T.Text ->
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  TripOf m s a b ->
  a ->
  Either RoundTripFailure (b, Plain.Encoding, BSL.ByteString)
embedTripLabelExtra lbl encVersion decVersion (Trip _generator encoder decoder dropper) s = result
  where
    mkFailure = RoundTripFailure encVersion decVersion encoding encodedBytes Nothing
    result =
      case decodeFullDecoder decVersion lbl decoder encodedBytes of
        Right val
          | Nothing <- mDropperError ->
              let flatTerm = CBOR.toFlatTerm encoding
               in case CBOR.fromFlatTerm (toPlainDecoder decVersion decoder) flatTerm of
                    Left _err ->
                      -- Until we switch to a release of cborg that includes a fix for this issue:
                      -- https://github.com/well-typed/cborg/issues/324
                      -- We can't rely on FlatTerm decoding
                      -- Left $ mkFailure (Just $ "fromFlatTerm error:" <> err) Nothing Nothing
                      Right (val, encoding, encodedBytes)
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
          --  else Left $ mkFailure Nothing Nothing
          | Just err <- mDropperError -> Left $ mkFailure Nothing (Just err) Nothing
        Left err ->
          -- In case of failure we only record dropper error if it differs from the
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
