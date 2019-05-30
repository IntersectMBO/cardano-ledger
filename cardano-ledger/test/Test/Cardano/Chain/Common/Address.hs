{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Common.Address
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog ((===), forAll, property)

import Cardano.Chain.Common (addrNetworkMagic)

import Test.Cardano.Chain.Common.Gen (genNetworkMagic, genAddressWithNM)
import Test.Options (TSGroup, TSProperty, withTestsTS)


ts_prop_addressNetworkMagicIdentity :: TSProperty
ts_prop_addressNetworkMagicIdentity = withTestsTS 1000 . property $ do
  nm <- forAll genNetworkMagic
  addr <- forAll (genAddressWithNM nm)
  nm === addrNetworkMagic addr

tests :: TSGroup
tests = $$discoverPropArg
