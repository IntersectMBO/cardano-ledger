{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Genesis.Json
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)
import qualified Hedgehog as H

import Test.Cardano.Chain.Genesis.Example
  (exampleStaticConfig_GCSpec, exampleStaticConfig_GCSrc)
import Test.Cardano.Chain.Genesis.Gen
  ( genGenesisAvvmBalances
  , genGenesisDelegation
  , genGenesisInitializer
  , genGenesisProtocolConstants
  , genStaticConfig
  )
import Test.Cardano.Crypto.Gen (feedPM)


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
-- GenesisAvvmBalances
--------------------------------------------------------------------------------

roundTripGenesisAvvmBalances :: Property
roundTripGenesisAvvmBalances =
  eachOf 100 genGenesisAvvmBalances roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisDelegation
--------------------------------------------------------------------------------

roundTripGenesisDelegation :: Property
roundTripGenesisDelegation =
  eachOf 100 (feedPM genGenesisDelegation) roundTripsAesonShow

--------------------------------------------------------------------------------
-- ProtocolConstants
--------------------------------------------------------------------------------

roundTripProtocolConstants :: Property
roundTripProtocolConstants =
  eachOf 1000 genGenesisProtocolConstants roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisInitializer
--------------------------------------------------------------------------------

roundTripGenesisInitializer :: Property
roundTripGenesisInitializer =
  eachOf 1000 genGenesisInitializer roundTripsAesonShow

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden <*> H.checkParallel
  $$discoverRoundTrip
