{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Common.Attributes
  ( tests,
  )
where

import Cardano.Prelude
import Hedgehog (assert, forAll, property)
import Hedgehog.Internal.Show (mkValue)
import Test.Cardano.Chain.Common.Gen (genAddrAttributes, genAttributes)
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, withTestsTS)

-- | Test wether instance Show (Attributes AddrAttributes) makes a string that
-- is useful for gebugging. More specifically, Hedgehog is able to parse it.
ts_prop_show_AddressAttributes_is_haskell :: TSProperty
ts_prop_show_AddressAttributes_is_haskell =
  withTestsTS 1000 . property $ do
    attr <- forAll $ genAttributes genAddrAttributes
    assert $ isJust $ mkValue attr

tests :: TSGroup
tests = $$discoverPropArg
