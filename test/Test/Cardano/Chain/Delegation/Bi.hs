{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Delegation.Bi
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.List ((!!))

import Hedgehog (Property)
import qualified Hedgehog as H

import Cardano.Chain.Delegation (unsafePayload)

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestBi, roundTripsBiBuildable)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import Test.Cardano.Chain.Delegation.Gen (genCertificate, genPayload)
import Test.Cardano.Crypto.Gen (feedPM)


--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

goldenCertificate :: Property
goldenCertificate = goldenTestBi cert "test/golden/bi/delegation/ProxySKHeavy"
  where cert = exampleCertificates !! 0

roundTripCertificateBi :: Property
roundTripCertificateBi =
  eachOf 200 (feedPM genCertificate) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------

goldenDlgPayload :: Property
goldenDlgPayload = goldenTestBi dp "test/golden/bi/delegation/DlgPayload"
  where dp = unsafePayload (take 4 exampleCertificates)

roundTripDlgPayloadBi :: Property
roundTripDlgPayloadBi = eachOf 100 (feedPM genPayload) roundTripsBiBuildable


tests :: IO Bool
tests = and <$> sequence
  [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
