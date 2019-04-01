{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Common.Compact
  ( tests
  )
where

import Cardano.Prelude

import Hedgehog
  ( Property
  , MonadTest
  , assert
  , property
  , discover
  , withTests
  , forAll
  , property
  , tripping
  )
import qualified Hedgehog as H

import Cardano.Chain.Common (fromCompactAddress, toCompactAddress)

import Test.Cardano.Chain.Common.Gen (genAddress)
import Test.Cardano.Prelude (discoverRoundTrip, eachOf)

--------------------------------------------------------------------------------
-- Compact Address
--------------------------------------------------------------------------------

roundTripCompactAddress :: Property
roundTripCompactAddress =
  eachOf 1000 genAddress (trippingCompact toCompactAddress fromCompactAddress)

prop_heapWordsSavingsCompactAddress :: Property
prop_heapWordsSavingsCompactAddress = withTests 1000 $ property $ do
  addr <- forAll genAddress
  let compactAddr = toCompactAddress addr
  assert $ heapWords compactAddr < heapWords addr

-------------------------------------------------------------------------------
-- Tripping util
-------------------------------------------------------------------------------

trippingCompact
  :: (HasCallStack, MonadTest m, Show a, Show b, Eq a)
  => (a -> b) -> (b -> a) -> a -> m ()
trippingCompact toCompact fromCompact x =
  tripping x toCompact (Identity . fromCompact)

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkParallel $$discover <*> H.checkParallel
  $$discoverRoundTrip
