{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.MempoolPayload.CBOR
  ( tests,
  )
where

import Cardano.Binary
  ( ByteSpan,
    Decoder,
    FromCBOR (..),
    ToCBOR,
    decodeFull,
    decodeFullDecoder,
    fromCBOR,
    serialize,
    slice,
    toCBOR,
  )
import Cardano.Prelude
import qualified Data.ByteString.Lazy as LBS
import Hedgehog (Property, tripping)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( goldenTestCBOR,
    goldenTestCBORExplicit,
    roundTripsCBORShow,
  )
import Test.Cardano.Chain.MempoolPayload.Example
  ( exampleMempoolPayload,
    exampleMempoolPayload1,
    exampleMempoolPayload2,
    exampleMempoolPayload3,
  )
import Test.Cardano.Chain.MempoolPayload.Gen (genMempoolPayload)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Serialises @f ()@ and uses that 'ByteString' as the annotation to splice
-- in.
fillInByteString ::
  forall f.
  (FromCBOR (f ByteSpan), ToCBOR (f ()), Functor f) =>
  f () ->
  f ByteString
fillInByteString a =
  either (panic . show) identity $ decodeFullDecoder mempty dec bytes
  where
    bytes :: LByteString
    bytes = serialize a

    dec :: Decoder s (f ByteString)
    dec = fmap (LBS.toStrict . slice bytes) <$> fromCBOR

-- | Variant of 'goldenTestCBOR' that does not use the @'ToCBOR' (f ())@
-- instance, but the @'ToCBOR' (f ByteString)@ instance. The latter instance
-- allows reusing the annotation when serialising instead of reserialising
-- from scratch.
filledInGoldenTestCBOR ::
  forall f.
  ( FromCBOR (f ()),
    ToCBOR (f ()),
    FromCBOR (f ByteSpan),
    ToCBOR (f ByteString),
    Functor f,
    Eq (f ()),
    Show (f ()),
    HasCallStack
  ) =>
  f () ->
  FilePath ->
  Property
filledInGoldenTestCBOR =
  goldenTestCBORExplicit
    (label $ Proxy @(f ()))
    (toCBOR . fillInByteString)
    fromCBOR

--------------------------------------------------------------------------------
-- MempoolPayload
--------------------------------------------------------------------------------

goldenMempoolPayload :: Property
goldenMempoolPayload =
  goldenTestCBOR
    exampleMempoolPayload
    "test/golden/cbor/mempoolpayload/MempoolPayload"

goldenMempoolPayloadFilledIn :: Property
goldenMempoolPayloadFilledIn =
  filledInGoldenTestCBOR
    exampleMempoolPayload
    "test/golden/cbor/mempoolpayload/MempoolPayload"

goldenMempoolPayload1 :: Property
goldenMempoolPayload1 =
  goldenTestCBOR
    exampleMempoolPayload1
    "test/golden/cbor/mempoolpayload/MempoolPayload1"

goldenMempoolPayload1FilledIn :: Property
goldenMempoolPayload1FilledIn =
  filledInGoldenTestCBOR
    exampleMempoolPayload1
    "test/golden/cbor/mempoolpayload/MempoolPayload1"

goldenMempoolPayload2 :: Property
goldenMempoolPayload2 =
  goldenTestCBOR
    exampleMempoolPayload2
    "test/golden/cbor/mempoolpayload/MempoolPayload2"

goldenMempoolPayload2FilledIn :: Property
goldenMempoolPayload2FilledIn =
  filledInGoldenTestCBOR
    exampleMempoolPayload2
    "test/golden/cbor/mempoolpayload/MempoolPayload2"

goldenMempoolPayload3 :: Property
goldenMempoolPayload3 =
  goldenTestCBOR
    exampleMempoolPayload3
    "test/golden/cbor/mempoolpayload/MempoolPayload3"

goldenMempoolPayload3FilledIn :: Property
goldenMempoolPayload3FilledIn =
  filledInGoldenTestCBOR
    exampleMempoolPayload3
    "test/golden/cbor/mempoolpayload/MempoolPayload3"

ts_roundTripMempoolPayload :: TSProperty
ts_roundTripMempoolPayload =
  eachOfTS 200 (feedPM genMempoolPayload) roundTripsCBORShow

ts_roundTripMempoolPayloadFilledIn :: TSProperty
ts_roundTripMempoolPayloadFilledIn =
  eachOfTS 200 (feedPM genMempoolPayload) $ \x ->
    tripping x (serialize . fillInByteString) decodeFull

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
