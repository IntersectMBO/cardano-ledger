{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Update.Json
  ( tests
  )
where

import Test.Cardano.Prelude

import Test.Cardano.Chain.Update.Gen (genProtocolParameters, genSoftforkRule)
import Test.Options (TSGroup, TSProperty, eachOfTS)


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

tests :: TSGroup
tests = $$discoverRoundTripArg
