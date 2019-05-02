{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.UTxO.Json
  ( tests
  )
where
import Cardano.Prelude

import qualified Data.Set as S
import Hedgehog (Group, Property)
import qualified Hedgehog as H

import Cardano.Chain.UTxO (UTxOConfiguration(..))

import Test.Cardano.Chain.Common.Example
  ( exampleAddress
  , exampleAddress1
  , exampleAddress2
  , exampleAddress3
  , exampleAddress4
  )
import Test.Cardano.Chain.UTxO.Gen (genUTxOConfiguration)
import Test.Cardano.Prelude
  ( discoverGolden
  , discoverRoundTripArg
  , goldenTestJSONPretty
  , roundTripsAesonShow
  )
import Test.Options (TestScenario, TSProperty, eachOfTS)

-------------------------------------------------------------------------------
-- UTxOConfiguration
-------------------------------------------------------------------------------

goldenUTxOConfiguration0 :: Property
goldenUTxOConfiguration0 = goldenTestJSONPretty
  exampleUTxOConfiguration0
  "test/golden/json/utxo/UTxOConfiguration0"

goldenUTxOConfiguration1 :: Property
goldenUTxOConfiguration1 =
    goldenTestJSONPretty exampleUTxOConfiguration1
        "test/golden/json/utxo/UTxOConfiguration1"

goldenUTxOConfiguration2 :: Property
goldenUTxOConfiguration2 =
    goldenTestJSONPretty exampleUTxOConfiguration2
        "test/golden/json/utxo/UTxOConfiguration2"

ts_roundTripUTxOConfiguration :: TSProperty
ts_roundTripUTxOConfiguration = eachOfTS 200 genUTxOConfiguration roundTripsAesonShow


-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

exampleUTxOConfiguration0 :: UTxOConfiguration
exampleUTxOConfiguration0 = UTxOConfiguration 99 talsa
  where talsa = S.fromList [exampleAddress]

exampleUTxOConfiguration1 :: UTxOConfiguration
exampleUTxOConfiguration1 = UTxOConfiguration 9 talsa
  where
    talsa = S.fromList [exampleAddress1, exampleAddress2, exampleAddress3]

exampleUTxOConfiguration2 :: UTxOConfiguration
exampleUTxOConfiguration2 = UTxOConfiguration 700 talsa
  where
    talsa = S.fromList [exampleAddress4, exampleAddress]

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: TestScenario -> IO Bool
tests ts = (&&) <$> H.checkSequential $$discoverGolden
                <*> H.checkParallel (($$discoverRoundTripArg :: TestScenario -> Group) ts)
