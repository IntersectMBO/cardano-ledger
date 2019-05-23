{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Delegation.CBOR
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.List ((!!))

import Hedgehog (Property)

import Cardano.Chain.Delegation (unsafePayload)

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestCBOR, roundTripsCBORBuildable)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import Test.Cardano.Chain.Delegation.Gen (genCertificate, genPayload)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

goldenCertificate :: Property
goldenCertificate = goldenTestCBOR
  cert
  "test/golden/cbor/delegation/ProxyVKHeavy"
  where cert = exampleCertificates !! 0

ts_roundTripCertificateCBOR :: TSProperty
ts_roundTripCertificateCBOR =
  eachOfTS 200 (feedPM genCertificate) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------

goldenDlgPayload :: Property
goldenDlgPayload = goldenTestCBOR dp "test/golden/cbor/delegation/DlgPayload"
  where dp = unsafePayload (take 4 exampleCertificates)

ts_roundTripDlgPayloadCBOR :: TSProperty
ts_roundTripDlgPayloadCBOR =
  eachOfTS 100 (feedPM genPayload) roundTripsCBORBuildable


tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
