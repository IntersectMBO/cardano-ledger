{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Update.Json
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)
import qualified Hedgehog as H

import Test.Cardano.Chain.Update.Gen (genProtocolParameters, genSoftforkRule)


--------------------------------------------------------------------------------
-- ProtocolParameters
--------------------------------------------------------------------------------

roundTripProtocolParameters :: Property
roundTripProtocolParameters =
  eachOf 1000 genProtocolParameters roundTripsAesonBuildable

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
