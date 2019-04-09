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
import Test.Options (TestScenario, TSProperty, eachOfTS)


--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

goldenCertificate :: Property
goldenCertificate = goldenTestBi cert "test/golden/bi/delegation/ProxyVKHeavy"
  where cert = exampleCertificates !! 0

ts_roundTripCertificateBi :: TSProperty
ts_roundTripCertificateBi =
  eachOfTS 200 (feedPM genCertificate) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------

goldenDlgPayload :: Property
goldenDlgPayload = goldenTestBi dp "test/golden/bi/delegation/DlgPayload"
  where dp = unsafePayload (take 4 exampleCertificates)

ts_roundTripDlgPayloadBi :: TSProperty
ts_roundTripDlgPayloadBi = eachOfTS 100 (feedPM genPayload) roundTripsBiBuildable


tests :: TestScenario -> IO Bool
tests ts = and <$> sequence
  [ H.checkSequential $$discoverGolden
  , H.checkParallel (($$discoverRoundTripArg :: TestScenario -> H.Group) ts)]
