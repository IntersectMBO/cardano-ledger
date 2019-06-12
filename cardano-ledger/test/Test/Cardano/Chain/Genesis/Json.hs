{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Genesis.Json
  ( tests
  ) where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)

import Test.Cardano.Chain.Genesis.Example (exampleGenesisData0)
import Test.Cardano.Chain.Genesis.Gen
  ( genGenesisAvvmBalances
  , genGenesisInitializer
  , genGenesisNonAvvmBalances
  , genGenesisKeyHashes
  )
import Test.Cardano.Chain.Delegation.Gen (genCanonicalCertificate)
import Test.Cardano.Chain.Update.Gen
  (genProtocolVersion, genCanonicalProtocolParameters)
import Test.Cardano.Chain.Genesis.Gen
  (genCanonicalGenesisData, genCanonicalGenesisDelegation)
import Test.Cardano.Crypto.Gen (feedPM, genProtocolMagic)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


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

ts_roundTripCanonicalGenesisKeyHashes :: TSProperty
ts_roundTripCanonicalGenesisKeyHashes =
  eachOfTS 100 genGenesisKeyHashes roundTripsCanonicalJsonPretty

ts_roundTripCanonicalProtocolParameters :: TSProperty
ts_roundTripCanonicalProtocolParameters =
  eachOfTS 100 genCanonicalProtocolParameters roundTripsCanonicalJsonPretty

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

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverPropArg]
