{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Update.Json
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Hedgehog as H

import Test.Cardano.Chain.Update.Gen (genProtocolParameters, genSoftforkRule)
import Test.Options (TestScenario, TSProperty, eachOfTS)


--------------------------------------------------------------------------------
-- ProtocolParameters
--------------------------------------------------------------------------------

ts_roundTripProtocolParameters :: TSProperty
ts_roundTripProtocolParameters =
  eachOfTS 1000 genProtocolParameters roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

ts_roundTripSoftforkRule :: TSProperty
ts_roundTripSoftforkRule = eachOfTS 1000 genSoftforkRule roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: TestScenario -> IO Bool
tests ts = H.checkParallel (($$discoverRoundTripArg :: TestScenario -> H.Group) ts)
