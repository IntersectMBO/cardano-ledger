{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Cardano.Chain.Common.Json
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)

import Cardano.Chain.Common
  ( LovelacePortion(..)
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  , mkKnownLovelace
  )

import Test.Cardano.Chain.Common.Example
  ( exampleAddress
  , exampleAddress1
  , exampleAddress2
  , exampleAddress3
  , exampleAddress4
  )
import Test.Cardano.Chain.Common.Gen
  (genAddress, genLovelace, genLovelacePortion, genTxFeePolicy, genTxSizeLinear)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

golden_Address0 :: Property
golden_Address0 =
    goldenTestJSON
        exampleAddress
        "test/golden/json/common/Address0"

golden_Address1 :: Property
golden_Address1 =
    goldenTestJSON
        exampleAddress1
        "test/golden/json/common/Address1"

golden_Address2 :: Property
golden_Address2 =
    goldenTestJSON
        exampleAddress2
        "test/golden/json/common/Address2"

golden_Address3 :: Property
golden_Address3 =
    goldenTestJSON
        exampleAddress3
        "test/golden/json/common/Address3"

golden_Address4 :: Property
golden_Address4 =
    goldenTestJSON
        exampleAddress4
        "test/golden/json/common/Address4"

ts_roundTripAddressShow :: TSProperty
ts_roundTripAddressShow =
    eachOfTS 100 genAddress roundTripsAesonShow

ts_roundTripAddressBuildable :: TSProperty
ts_roundTripAddressBuildable =
    eachOfTS 100 genAddress roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Lovelace
--------------------------------------------------------------------------------

golden_Lovelace :: Property
golden_Lovelace = goldenTestJSON ll "test/golden/json/common/Lovelace"
  where ll = mkKnownLovelace @77

ts_roundTripLovelace :: TSProperty
ts_roundTripLovelace = eachOfTS 1000 genLovelace roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- LovelacePortion
--------------------------------------------------------------------------------

golden_LovelacePortion :: Property
golden_LovelacePortion = goldenTestJSON llp "test/golden/json/common/LovelacePortion"
  where llp = LovelacePortion 3720

ts_roundTripLovelacePortion :: TSProperty
ts_roundTripLovelacePortion = eachOfTS 1000 genLovelacePortion roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------

golden_TxFeePolicy_Linear :: Property
golden_TxFeePolicy_Linear = goldenTestJSON tfp "test/golden/json/common/TxFeePolicy_Linear"
  where
    tfp = TxFeePolicyTxSizeLinear (TxSizeLinear c1 c2)
    c1 = mkKnownLovelace @99
    c2 = mkKnownLovelace @777

ts_roundTripTxFeePolicy :: TSProperty
ts_roundTripTxFeePolicy = eachOfTS 1000 genTxFeePolicy roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- TxSizeLinear
--------------------------------------------------------------------------------

golden_TxSizeLinear :: Property
golden_TxSizeLinear = goldenTestJSON tsl "test/golden/json/common/TxSizeLinear"
  where
    tsl = TxSizeLinear c1 c2
    c1 = mkKnownLovelace @99
    c2 = mkKnownLovelace @777

ts_roundTripTxSizeLinear :: TSProperty
ts_roundTripTxSizeLinear = eachOfTS 1000 genTxSizeLinear roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
