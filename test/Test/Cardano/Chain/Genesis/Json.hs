{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Genesis.Json
  ( tests
  ) where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)
import qualified Hedgehog as H

import Test.Cardano.Chain.Genesis.Example
  (exampleGenesisData0, exampleStaticConfig_GCSpec, exampleStaticConfig_GCSrc)
import Test.Cardano.Chain.Genesis.Gen
  ( genGenesisAvvmBalances
  , genGenesisDelegation
  , genGenesisInitializer
  , genGenesisNonAvvmBalances
  , genGenesisWStakeholders
  , genStaticConfig
  )
import Test.Cardano.Chain.Delegation.Gen (genCanonicalCertificate)
import Test.Cardano.Chain.Update.Gen (genProtocolVersion, genCanonicalProtocolParameters)
import Test.Cardano.Chain.Genesis.Gen (genCanonicalGenesisData, genCanonicalGenesisDelegation, genSafeProxyCert)
import Test.Cardano.Crypto.Gen (feedPM, genProtocolMagic, genVerificationKey)
import Test.Options (TestScenario, TSProperty, eachOfTS)




--------------------------------------------------------------------------------
-- StaticConfig
--------------------------------------------------------------------------------

goldenStaticConfig_GCSpec :: Property
goldenStaticConfig_GCSpec = goldenTestJSONPretty
  exampleStaticConfig_GCSpec
  "test/golden/json/genesis/StaticConfig_GCSpec"

goldenStaticConfig_GCSrc :: Property
goldenStaticConfig_GCSrc = goldenTestJSONPretty
  exampleStaticConfig_GCSrc
  "test/golden/json/genesis/StaticConfig_GCSrc"

ts_roundTripStaticConfig :: TSProperty
ts_roundTripStaticConfig = eachOfTS 100 (feedPM genStaticConfig) roundTripsAesonShow

--------------------------------------------------------------------------------
-- JSON Canonical Tests
--------------------------------------------------------------------------------

ts_roundTripCanonicalCertificate :: TSProperty
ts_roundTripCanonicalCertificate =
  eachOfTS 100 (feedPM genCanonicalCertificate) roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisAvvmBalances :: TSProperty
ts_roundTripCanonicalGenesisAvvmBalances =
  eachOfTS 100 genGenesisAvvmBalances roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisData :: TSProperty
ts_roundTripCanonicalGenesisData =
  eachOfTS 100 (feedPM genCanonicalGenesisData) roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisDelegation :: TSProperty
ts_roundTripCanonicalGenesisDelegation =
  eachOfTS 100 (feedPM genCanonicalGenesisDelegation) roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisNonAvvmBalances :: TSProperty
ts_roundTripCanonicalGenesisNonAvvmBalances =
  eachOfTS 100 genGenesisNonAvvmBalances roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisWStakeholders :: TSProperty
ts_roundTripCanonicalGenesisWStakeholders =
  eachOfTS 100 genGenesisWStakeholders roundTripsCanonicalJsonPretty

ts_roundTripCanonicalProtocolParameters :: TSProperty
ts_roundTripCanonicalProtocolParameters =
  eachOfTS 100 genCanonicalProtocolParameters roundTripsCanonicalJsonPretty

ts_roundTripCanonicalSafeProxyCert :: TSProperty
ts_roundTripCanonicalSafeProxyCert =
  eachOfTS 100 genSafeProxyCert roundTripsCanonicalJsonPretty

--------------------------------------------------------------------------------
-- GenesisAvvmBalances
--------------------------------------------------------------------------------

ts_roundTripGenesisAvvmBalances :: TSProperty
ts_roundTripGenesisAvvmBalances =
  eachOfTS 100 genGenesisAvvmBalances roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisData (Canonical JSON)
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `GenesisData` canonical JSON format, the `RequiresNetworkMagic` field
-- defaults to `RequiresMagic`.

golden_GenesisData0Dec :: Property
golden_GenesisData0Dec =
  goldenTestCanonicalJSONDec exampleGenesisData0
    "test/golden/json/genesis/GenesisData0_Legacy_HasNetworkMagic"

--------------------------------------------------------------------------------
-- GenesisDelegation
--------------------------------------------------------------------------------

ts_roundTripGenesisDelegation :: TSProperty
ts_roundTripGenesisDelegation =
  eachOfTS 100 (feedPM genGenesisDelegation) roundTripsAesonShow

ts_roundTripCanonicalVerificationKey :: TSProperty
ts_roundTripCanonicalVerificationKey =
  eachOfTS 100 genVerificationKey roundTripsCanonicalJsonPretty

--------------------------------------------------------------------------------
-- GenesisInitializer
--------------------------------------------------------------------------------

ts_roundTripGenesisInitializer :: TSProperty
ts_roundTripGenesisInitializer =
  eachOfTS 1000 genGenesisInitializer roundTripsAesonShow

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

ts_roundTripProtocolVersion :: TSProperty
ts_roundTripProtocolVersion =
  eachOfTS 100 genProtocolVersion roundTripsAesonShow

--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

ts_roundTripProtocolMagic :: TSProperty
ts_roundTripProtocolMagic =
  eachOfTS 100 genProtocolMagic roundTripsAesonShow

tests :: TestScenario -> IO Bool
tests ts = (&&) <$> H.checkSequential $$discoverGolden
                <*> H.checkParallel (($$discoverRoundTripArg :: TestScenario -> H.Group) ts)
