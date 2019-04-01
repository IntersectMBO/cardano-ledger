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

import Hedgehog
  (Property, property, discover, (===), withTests, forAll)
import qualified Hedgehog as H

import Cardano.Chain.Common
  ( addrNetworkMagic
  )

import Test.Cardano.Chain.Common.Gen (genNetworkMagic, genAddressWithNM)

prop_addressNetworkMagicIdentity :: Property
prop_addressNetworkMagicIdentity = withTests 1000 . property $ do
  nm <- forAll genNetworkMagic
  addr <- forAll (genAddressWithNM nm)
  nm === addrNetworkMagic addr

tests :: IO Bool
tests = H.checkParallel $$(discover)
