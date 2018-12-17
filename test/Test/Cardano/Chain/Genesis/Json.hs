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
import Test.Cardano.Crypto.Gen (feedPM, genProtocolMagic, genPublicKey)




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

roundTripStaticConfig :: Property
roundTripStaticConfig = eachOf 100 (feedPM genStaticConfig) roundTripsAesonShow

--------------------------------------------------------------------------------
-- JSON Canonical Tests
--------------------------------------------------------------------------------

roundTripCanonicalCertificate :: Property
roundTripCanonicalCertificate =
  eachOf 100 (feedPM genCanonicalCertificate) roundTripsCanonicalJsonPretty

roundTripCanonicalGenesisAvvmBalances :: Property
roundTripCanonicalGenesisAvvmBalances =
  eachOf 100 genGenesisAvvmBalances roundTripsCanonicalJsonPretty

roundTripCanonicalGenesisData :: Property
roundTripCanonicalGenesisData =
  eachOf 100 (feedPM genCanonicalGenesisData) roundTripsCanonicalJsonPretty

roundTripCanonicalGenesisDelegation :: Property
roundTripCanonicalGenesisDelegation =
  eachOf 100 (feedPM genCanonicalGenesisDelegation) roundTripsCanonicalJsonPretty

roundTripCanonicalGenesisNonAvvmBalances :: Property
roundTripCanonicalGenesisNonAvvmBalances =
  eachOf 100 genGenesisNonAvvmBalances roundTripsCanonicalJsonPretty

roundTripCanonicalGenesisWStakeholders :: Property
roundTripCanonicalGenesisWStakeholders =
  eachOf 100 genGenesisWStakeholders roundTripsCanonicalJsonPretty

roundTripCanonicalProtocolParameters :: Property
roundTripCanonicalProtocolParameters =
  eachOf 100 genCanonicalProtocolParameters roundTripsCanonicalJsonPretty

roundTripCanonicalSafeProxyCert :: Property
roundTripCanonicalSafeProxyCert =
  eachOf 100 genSafeProxyCert roundTripsCanonicalJsonPretty

--------------------------------------------------------------------------------
-- GenesisAvvmBalances
--------------------------------------------------------------------------------

roundTripGenesisAvvmBalances :: Property
roundTripGenesisAvvmBalances =
  eachOf 100 genGenesisAvvmBalances roundTripsAesonShow

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

roundTripGenesisDelegation :: Property
roundTripGenesisDelegation =
  eachOf 100 (feedPM genGenesisDelegation) roundTripsAesonShow

roundTripCanonicalPublicKey :: Property
roundTripCanonicalPublicKey =
  eachOf 100 genPublicKey roundTripsCanonicalJsonPretty

--------------------------------------------------------------------------------
-- GenesisInitializer
--------------------------------------------------------------------------------

roundTripGenesisInitializer :: Property
roundTripGenesisInitializer =
  eachOf 1000 genGenesisInitializer roundTripsAesonShow

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripProtocolVersion :: Property
roundTripProtocolVersion =
  eachOf 100 genProtocolVersion roundTripsAesonShow

--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

roundTripProtocolMagic :: Property
roundTripProtocolMagic =
  eachOf 100 genProtocolMagic roundTripsAesonShow

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden <*> H.checkParallel
  $$discoverRoundTrip
