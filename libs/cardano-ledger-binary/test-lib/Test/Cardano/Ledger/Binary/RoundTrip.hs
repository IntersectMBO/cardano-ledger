{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines reusable abstractions for testing RoundTrip properties of CBOR instances
module Test.Cardano.Ledger.Binary.RoundTrip
  ( roundTripSpec,
    roundTripExpectation,
    RoundTripFailure (..),
    Trip (..),
    cborTrip,
    roundTrip,
    roundTripTwiddled,
    roundTripAnn,
    roundTripAnnTwiddled,
    embedTrip,
    embedTripAnn,
    embedTripLabel,
  )
where

import Cardano.Ledger.Binary
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as Text
import Data.Typeable
import Test.Cardano.Ledger.Binary.TreeDiff (showExpr, showHexBytesGrouped)
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (label)

-- =====================================================================

-- | Tests the roundtrip property using QuickCheck generators
roundTripSpec ::
  forall t.
  (Show t, Eq t, Typeable t, Arbitrary t) =>
  Version ->
  Trip t t ->
  Spec
roundTripSpec version trip =
  prop (show (typeRep $ Proxy @t)) $ roundTripExpectation version trip

roundTripExpectation ::
  (Show t, Eq t, Typeable t) =>
  Version ->
  Trip t t ->
  t ->
  Expectation
roundTripExpectation version trip t =
  case roundTrip version trip t of
    Left err -> expectationFailure $ "Failed to deserialize encoded: " ++ show err
    Right tDecoded -> tDecoded `shouldBe` t

-- =====================================================================

data RoundTripFailure = RoundTripFailure
  { -- | Version that was used during encoding
    rtfVersion :: Version,
    -- | Produced encoding
    rtfEncoding :: Encoding,
    -- | Serialized encoding using the version in this failure
    rtfEncodedBytes :: BSL.ByteString,
    -- | Error received while decoding the produced bytes
    rtfDecoderError :: DecoderError
  }

instance Show RoundTripFailure where
  show RoundTripFailure {..} =
    unlines $
      [ show rtfVersion,
        showDecoderError rtfDecoderError,
        prettyTerm
      ]
        ++ showHexBytesGrouped bytes
    where
      bytes = BSL.toStrict rtfEncodedBytes
      prettyTerm =
        case decodeFullDecoder' rtfVersion "Term" decodeTerm bytes of
          Left err -> "Could not decode as Term: " ++ show err
          Right term -> showExpr term

-- | A definition of a CBOR trip through binary representation of one type to
-- another. In this module this is called an embed. When a source and target type is the
-- exect same one then it would be a dual and is expected to round trip.
data Trip a b = Trip
  { tripEncoder :: a -> Encoding,
    tripDecoder :: forall s. Decoder s b
  }

cborTrip :: (ToCBOR a, FromCBOR b) => Trip a b
cborTrip = Trip toCBOR fromCBOR

roundTrip :: Typeable t => Version -> Trip t t -> t -> Either RoundTripFailure t
roundTrip = embedTrip

roundTripTwiddled ::
  (Twiddle t, FromCBOR t) => Version -> t -> Gen (Either RoundTripFailure t)
roundTripTwiddled version x = do
  tw <- twiddle x
  pure (roundTrip version (Trip (const (encodeTerm tw)) fromCBOR) x)

roundTripAnn :: (ToCBOR t, FromCBOR (Annotator t)) => Version -> t -> Either RoundTripFailure t
roundTripAnn = embedTripAnn

roundTripAnnTwiddled ::
  (Twiddle t, FromCBOR (Annotator t)) => Version -> t -> Gen (Either RoundTripFailure t)
roundTripAnnTwiddled version x = do
  tw <- twiddle x
  pure (decodeAnn version (encodeTerm tw))

decodeAnn ::
  forall t.
  FromCBOR (Annotator t) =>
  Version ->
  Encoding ->
  Either RoundTripFailure t
decodeAnn version encoding =
  first (RoundTripFailure version encoding encodedBytes) $
    decodeFullAnnotator version (label (Proxy @(Annotator t))) fromCBOR encodedBytes
  where
    encodedBytes = serializeEncoding version encoding

embedTripLabel ::
  forall a b.
  Text.Text ->
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTripLabel lbl version (Trip encoder decoder) s =
  first (RoundTripFailure version encoding encodedBytes) $
    decodeFullDecoder version lbl decoder encodedBytes
  where
    encoding = encoder s
    encodedBytes = serializeEncoding version encoding

-- | Can we serialise a type, and then deserialise it as something else?
embedTrip :: forall a b. Typeable b => Version -> Trip a b -> a -> Either RoundTripFailure b
embedTrip = embedTripLabel (Text.pack (show (typeRep $ Proxy @b)))

embedTripAnn ::
  forall a b. (ToCBOR a, FromCBOR (Annotator b)) => Version -> a -> Either RoundTripFailure b
embedTripAnn version = decodeAnn version . toCBOR
