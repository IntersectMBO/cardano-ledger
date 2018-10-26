{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Delegation.Bi
       ( tests
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Data.List ((!!))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Chain.Delegation (Payload (..), ProxySKBlockInfo)

import           Test.Cardano.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Cardano.Chain.Delegation.Example (exampleLightDlgIndices,
                     exampleProxySKBlockInfo, staticHeavyDlgIndexes,
                     staticProxySKHeavys)
import           Test.Cardano.Chain.Delegation.Gen (genHeavyDlgIndex,
                     genLightDlgIndices, genPayload, genProxySKBlockInfo,
                     genProxySKHeavy)
import           Test.Cardano.Crypto.Gen (feedPM)

--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------
goldenDlgPayload :: Property
goldenDlgPayload = goldenTestBi dp "test/golden/bi/delegation/DlgPayload"
  where dp = UnsafePayload (take 4 staticProxySKHeavys)

roundTripDlgPayloadBi :: Property
roundTripDlgPayloadBi = eachOf 100 (feedPM genPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HeavyDlgIndex
--------------------------------------------------------------------------------
goldenHeavyDlgIndex :: Property
goldenHeavyDlgIndex = goldenTestBi hdi "test/golden/bi/delegation/HeavyDlgIndex"
  where hdi = staticHeavyDlgIndexes !! 0

roundTripHeavyDlgIndexBi :: Property
roundTripHeavyDlgIndexBi = eachOf 1000 genHeavyDlgIndex roundTripsBiBuildable

--------------------------------------------------------------------------------
-- LightDlgIndices
--------------------------------------------------------------------------------
goldenLightDlgIndices :: Property
goldenLightDlgIndices = goldenTestBi exampleLightDlgIndices
                                      "test/golden/bi/delegation/LightDlgIndices"

roundTripLightDlgIndicesBi :: Property
roundTripLightDlgIndicesBi = eachOf 1000 genLightDlgIndices roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProxySKBlockInfo
--------------------------------------------------------------------------------
goldenProxySKBlockInfo_Nothing :: Property
goldenProxySKBlockInfo_Nothing = goldenTestBi pskbi "test/golden/bi/delegation/ProxySKBlockInfo_Nothing"
  where pskbi = Nothing :: ProxySKBlockInfo

goldenProxySKBlockInfo_Just :: Property
goldenProxySKBlockInfo_Just = goldenTestBi exampleProxySKBlockInfo
                                            "test/golden/bi/delegation/ProxySKBlockInfo_Just"

roundTripProxySKBlockInfoBi :: Property
roundTripProxySKBlockInfoBi = eachOf 200 (feedPM genProxySKBlockInfo) roundTripsBiShow

--------------------------------------------------------------------------------
-- ProxySKHeavy
--------------------------------------------------------------------------------
goldenProxySKHeavy :: Property
goldenProxySKHeavy = goldenTestBi skh "test/golden/bi/delegation/ProxySKHeavy"
  where skh = staticProxySKHeavys !! 0

roundTripProxySKHeavyBi :: Property
roundTripProxySKHeavyBi = eachOf 200 (feedPM genProxySKHeavy) roundTripsBiBuildable

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    ]
