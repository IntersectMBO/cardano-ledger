{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Delegation.CBOR
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.List ((!!))

import Hedgehog (Property)

import Cardano.Chain.Delegation (Payload(..))

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestCBORAnnotated, roundTripsCBORAnnotatedBuildable, roundTripsCBORShow)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import Test.Cardano.Chain.Delegation.Gen
  (genCertificate, genError, genPayload)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

goldenCertificate :: Property
goldenCertificate = goldenTestCBORAnnotated
  cert
  "test/golden/cbor/delegation/Certificate"
  where cert = exampleCertificates !! 0

ts_roundTripCertificateCBOR :: TSProperty
ts_roundTripCertificateCBOR =
  eachOfTS 200 (feedPM genCertificate) roundTripsCBORAnnotatedBuildable


--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------

goldenDlgPayload :: Property
goldenDlgPayload = goldenTestCBORAnnotated dp "test/golden/cbor/delegation/DlgPayload"
  where dp = UnsafePayload (take 4 exampleCertificates)

ts_roundTripDlgPayloadCBOR :: TSProperty
ts_roundTripDlgPayloadCBOR =
  eachOfTS 100 (feedPM genPayload) roundTripsCBORAnnotatedBuildable


--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

ts_roundTripErrorCBOR :: TSProperty
ts_roundTripErrorCBOR =
  eachOfTS 100 genError roundTripsCBORShow


tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
