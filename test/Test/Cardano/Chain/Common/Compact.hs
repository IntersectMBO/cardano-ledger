{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Common.Compact
  ( tests
  )
where

import Cardano.Prelude

import Hedgehog (Property)
import qualified Hedgehog as H
import Hedgehog (MonadTest, tripping)

import Cardano.Chain.Common
  ( fromCompactAddress
  , toCompactAddress
  )

import Test.Cardano.Chain.Common.Gen (genAddress)
import Test.Cardano.Prelude
  ( discoverGolden
  , discoverRoundTrip
  , eachOf
  )

--------------------------------------------------------------------------------
-- Compact Address
--------------------------------------------------------------------------------

roundTripCompactAddress :: Property
roundTripCompactAddress =
  eachOf 1000 genAddress (trippingCompact toCompactAddress fromCompactAddress)

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
tests = (&&) <$> H.checkSequential $$discoverGolden <*> H.checkParallel
  $$discoverRoundTrip
