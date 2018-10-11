{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Update.Json
       ( tests
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Cardano.Chain.Update.Gen (genBlockVersionData,
                     genSoftforkRule)


--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData =
    eachOf 1000 genBlockVersionData roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 1000 genSoftforkRule roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
