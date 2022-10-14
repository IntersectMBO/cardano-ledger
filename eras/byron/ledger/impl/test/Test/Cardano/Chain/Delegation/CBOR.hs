{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Delegation.CBOR
  ( tests,
  )
where

import Cardano.Chain.Delegation (unsafePayload)
import Cardano.Prelude
import Data.List ((!!))
import GetDataFileName ((<:<))
import Hedgehog (Property)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import Test.Cardano.Chain.Delegation.Gen
  ( genCertificate,
    genError,
    genPayload,
  )
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Cardano.Ledger.Binary.Vintage.Helpers.GoldenRoundTrip
  ( goldenTestCBOR,
    roundTripsCBORBuildable,
    roundTripsCBORShow,
  )
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

goldenCertificate :: Property
goldenCertificate =
  goldenTestCBOR
    cert
    <:< "golden/cbor/delegation/Certificate"
  where
    cert = exampleCertificates !! 0

ts_roundTripCertificateCBOR :: TSProperty
ts_roundTripCertificateCBOR =
  eachOfTS 200 (feedPM genCertificate) roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------

goldenDlgPayload :: Property
goldenDlgPayload = goldenTestCBOR dp <:< "golden/cbor/delegation/DlgPayload"
  where
    dp = unsafePayload (take 4 exampleCertificates)

ts_roundTripDlgPayloadCBOR :: TSProperty
ts_roundTripDlgPayloadCBOR =
  eachOfTS 100 (feedPM genPayload) roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

ts_roundTripErrorCBOR :: TSProperty
ts_roundTripErrorCBOR =
  eachOfTS 100 genError roundTripsCBORShow

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
