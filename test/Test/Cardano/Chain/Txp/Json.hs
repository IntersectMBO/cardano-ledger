{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Txp.Json
       ( tests
       ) where
import           Cardano.Prelude

import qualified Data.Set as S
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Chain.Txp (TxpConfiguration (..))

import           Test.Cardano.Chain.Txp.Gen (genTxpConfiguration)
import           Test.Cardano.Core.ExampleHelpers (exampleAddress, exampleAddress1,
                     exampleAddress2, exampleAddress3, exampleAddress4)
import           Test.Cardano.Util.Golden (discoverGolden, eachOf, goldenTestJSON)
import           Test.Cardano.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- TxpConfiguration
-------------------------------------------------------------------------------

golden_TxpConfiguration0 :: Property
golden_TxpConfiguration0 =
    goldenTestJSON exampleTxpConfiguration0
        "test/golden/json/TxpConfiguration0"

golden_TxpConfiguration1 :: Property
golden_TxpConfiguration1 =
    goldenTestJSON exampleTxpConfiguration1
        "test/golden/json/TxpConfiguration1"

golden_TxpConfiguration2 :: Property
golden_TxpConfiguration2 =
    goldenTestJSON exampleTxpConfiguration2
        "test/golden/json/TxpConfiguration2"

roundTripTxpConfiguration :: Property
roundTripTxpConfiguration =
    eachOf 200 genTxpConfiguration roundTripsAesonShow

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

exampleTxpConfiguration0 :: TxpConfiguration
exampleTxpConfiguration0 = TxpConfiguration 99 talsa
  where
    talsa = S.fromList [exampleAddress]

exampleTxpConfiguration1 :: TxpConfiguration
exampleTxpConfiguration1 = TxpConfiguration 9 talsa
  where
    talsa = S.fromList [exampleAddress1, exampleAddress2, exampleAddress3]

exampleTxpConfiguration2 :: TxpConfiguration
exampleTxpConfiguration2 = TxpConfiguration 700 talsa
  where
    talsa = S.fromList [exampleAddress4, exampleAddress]

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
