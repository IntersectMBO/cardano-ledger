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

import Hedgehog
  (property, (===), forAll)
import qualified Hedgehog as H

import Cardano.Chain.Common
  ( addrNetworkMagic
  )

import Test.Cardano.Chain.Common.Gen (genNetworkMagic, genAddressWithNM)
import Test.Options (TestScenario, TSProperty, withTestsTS)

ts_prop_addressNetworkMagicIdentity :: TSProperty
ts_prop_addressNetworkMagicIdentity = withTestsTS 1000 . property $ do
  nm <- forAll genNetworkMagic
  addr <- forAll (genAddressWithNM nm)
  nm === addrNetworkMagic addr

tests :: TestScenario -> IO Bool
tests ts = H.checkParallel (($$discoverPropArg :: TestScenario -> H.Group) ts)
