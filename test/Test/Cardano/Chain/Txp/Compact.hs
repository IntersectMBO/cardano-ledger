{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Txp.Compact
  ( tests
  )
where

import Cardano.Prelude

import Hedgehog (MonadTest, Property, tripping)
import qualified Hedgehog as H

import Cardano.Chain.Txp
  ( fromCompactTxId
  , toCompactTxId
  , fromCompactTxIn
  , toCompactTxIn
  , fromCompactTxOut
  , toCompactTxOut
  )

import Test.Cardano.Chain.Txp.Gen
  ( genTxId
  , genTxIn
  , genTxOut
  )
import Test.Cardano.Prelude
  ( discoverGolden
  , discoverRoundTrip
  , eachOf
  )

--------------------------------------------------------------------------------
-- Compact TxIn
--------------------------------------------------------------------------------

roundTripCompactTxIn :: Property
roundTripCompactTxIn =
  eachOf 1000 genTxIn (trippingCompact toCompactTxIn fromCompactTxIn)

--------------------------------------------------------------------------------
-- Compact TxId
--------------------------------------------------------------------------------

roundTripCompactTxId :: Property
roundTripCompactTxId =
  eachOf 1000 genTxId (trippingCompact toCompactTxId fromCompactTxId)

--------------------------------------------------------------------------------
-- Compact TxOut
--------------------------------------------------------------------------------

roundTripCompactTxOut :: Property
roundTripCompactTxOut =
  eachOf 1000 genTxOut (trippingCompact toCompactTxOut fromCompactTxOut)

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
