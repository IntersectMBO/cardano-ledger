{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.MempoolPayload.CBOR (
  tests,
) where

import Cardano.Ledger.Binary (
  ByteSpan,
  DecCBOR (..),
  Decoder,
  EncCBOR,
  byronProtVer,
  decCBOR,
  decodeFull,
  decodeFullDecoder,
  encCBOR,
  serialize,
  slice,
 )
import Cardano.Prelude
import qualified Data.ByteString.Lazy as LBS
import Hedgehog (Property, tripping)
import Test.Cardano.Chain.MempoolPayload.Example (
  exampleMempoolPayload,
  exampleMempoolPayload1,
  exampleMempoolPayload2,
  exampleMempoolPayload3,
 )
import Test.Cardano.Chain.MempoolPayload.Gen (genMempoolPayload)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Cardano.Ledger.Binary.Vintage.Helpers.GoldenRoundTrip (
  goldenTestCBOR,
  goldenTestCBORExplicit,
  roundTripsCBORShow,
 )
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Serialises @f ()@ and uses that 'ByteString' as the annotation to splice
-- in.
fillInByteString ::
  forall f.
  (DecCBOR (f ByteSpan), EncCBOR (f ()), Functor f) =>
  f () ->
  f ByteString
fillInByteString a =
  either (panic . show) identity $ decodeFullDecoder byronProtVer mempty dec bytes
  where
    bytes :: LByteString
    bytes = serialize byronProtVer a

    dec :: Decoder s (f ByteString)
    dec = fmap (LBS.toStrict . slice bytes) <$> decCBOR

-- | Variant of 'goldenTestCBOR' that does not use the @'EncCBOR' (f ())@
-- instance, but the @'EncCBOR' (f ByteString)@ instance. The latter instance
-- allows reusing the annotation when serialising instead of reserialising
-- from scratch.
filledInGoldenTestCBOR ::
  forall f.
  ( DecCBOR (f ())
  , EncCBOR (f ())
  , DecCBOR (f ByteSpan)
  , EncCBOR (f ByteString)
  , Functor f
  , Eq (f ())
  , Show (f ())
  , HasCallStack
  ) =>
  f () ->
  FilePath ->
  Property
filledInGoldenTestCBOR =
  goldenTestCBORExplicit
    (label $ Proxy @(f ()))
    (encCBOR . fillInByteString)
    decCBOR

--------------------------------------------------------------------------------
-- MempoolPayload
--------------------------------------------------------------------------------

goldenMempoolPayload :: Property
goldenMempoolPayload =
  goldenTestCBOR
    exampleMempoolPayload
    "golden/cbor/mempoolpayload/MempoolPayload"

goldenMempoolPayloadFilledIn :: Property
goldenMempoolPayloadFilledIn =
  filledInGoldenTestCBOR
    exampleMempoolPayload
    "golden/cbor/mempoolpayload/MempoolPayload"

goldenMempoolPayload1 :: Property
goldenMempoolPayload1 =
  goldenTestCBOR
    exampleMempoolPayload1
    "golden/cbor/mempoolpayload/MempoolPayload1"

goldenMempoolPayload1FilledIn :: Property
goldenMempoolPayload1FilledIn =
  filledInGoldenTestCBOR
    exampleMempoolPayload1
    "golden/cbor/mempoolpayload/MempoolPayload1"

goldenMempoolPayload2 :: Property
goldenMempoolPayload2 =
  goldenTestCBOR
    exampleMempoolPayload2
    "golden/cbor/mempoolpayload/MempoolPayload2"

goldenMempoolPayload2FilledIn :: Property
goldenMempoolPayload2FilledIn =
  filledInGoldenTestCBOR
    exampleMempoolPayload2
    "golden/cbor/mempoolpayload/MempoolPayload2"

goldenMempoolPayload3 :: Property
goldenMempoolPayload3 =
  goldenTestCBOR
    exampleMempoolPayload3
    "golden/cbor/mempoolpayload/MempoolPayload3"

goldenMempoolPayload3FilledIn :: Property
goldenMempoolPayload3FilledIn =
  filledInGoldenTestCBOR
    exampleMempoolPayload3
    "golden/cbor/mempoolpayload/MempoolPayload3"

ts_roundTripMempoolPayload :: TSProperty
ts_roundTripMempoolPayload =
  eachOfTS 200 (feedPM genMempoolPayload) roundTripsCBORShow

ts_roundTripMempoolPayloadFilledIn :: TSProperty
ts_roundTripMempoolPayloadFilledIn =
  eachOfTS 200 (feedPM genMempoolPayload) $ \x ->
    tripping x (serialize byronProtVer . fillInByteString) (decodeFull byronProtVer)

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
