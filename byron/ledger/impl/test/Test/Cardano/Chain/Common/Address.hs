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

import Hedgehog ((===), cover, forAll, property)

import Cardano.Chain.Common (addrNetworkMagic, isRedeemAddress)

import Test.Cardano.Chain.Common.Gen (genAddress, genAddressWithNM, genNetworkMagic)
import Test.Options (TSGroup, TSProperty, withTestsTS)


ts_prop_addressNetworkMagicIdentity :: TSProperty
ts_prop_addressNetworkMagicIdentity =
  withTestsTS 1000 . property $ do
    nm <- forAll genNetworkMagic
    addr <- forAll (genAddressWithNM nm)
    nm === addrNetworkMagic addr

ts_prop_isRedeemAddress :: TSProperty
ts_prop_isRedeemAddress =
  withTestsTS 1000 . property $ do
    addr <- forAll genAddress
    cover 30 "Redeem Address" $ isRedeemAddress addr
    cover 30 "Pubkey Address" $ not (isRedeemAddress addr)

tests :: TSGroup
tests = $$discoverPropArg
