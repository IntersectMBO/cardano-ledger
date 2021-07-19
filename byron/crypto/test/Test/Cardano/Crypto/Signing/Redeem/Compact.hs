{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Signing.Redeem.Compact
  ( tests,
  )
where

import Cardano.Crypto.Signing.Redeem
  ( fromCompactRedeemVerificationKey,
    toCompactRedeemVerificationKey,
  )
import Cardano.Prelude
import Hedgehog (MonadTest, Property, checkParallel, tripping)
import Test.Cardano.Crypto.Gen (genRedeemVerificationKey)
import Test.Cardano.Prelude

--------------------------------------------------------------------------------
-- Compact RedeemVerificationKey
--------------------------------------------------------------------------------

roundTripCompactRedeemVerificationKey :: Property
roundTripCompactRedeemVerificationKey =
  eachOf
    1000
    genRedeemVerificationKey
    (trippingCompact toCompactRedeemVerificationKey fromCompactRedeemVerificationKey)

-------------------------------------------------------------------------------
-- Tripping util
-------------------------------------------------------------------------------

trippingCompact ::
  (HasCallStack, MonadTest m, Show a, Show b, Eq a) =>
  (a -> b) ->
  (b -> a) ->
  a ->
  m ()
trippingCompact toCompact fromCompact x =
  tripping x toCompact (Identity . fromCompact)

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discoverRoundTrip
