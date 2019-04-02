{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Txp.Compact
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
  ( discoverRoundTrip
  , eachOf
  )

--------------------------------------------------------------------------------
-- Compact TxIn
--------------------------------------------------------------------------------

roundTripCompactTxIn :: Property
roundTripCompactTxIn =
  eachOf 1000 genTxIn (trippingCompact toCompactTxIn fromCompactTxIn)

prop_heapWordsSavingsCompactTxIn :: Property
prop_heapWordsSavingsCompactTxIn = withTests 1000 $ property $ do
  txIn <- forAll genTxIn
  let compactTxIn = toCompactTxIn txIn
  assert $ heapWords compactTxIn < heapWords txIn

--------------------------------------------------------------------------------
-- Compact TxId
--------------------------------------------------------------------------------

roundTripCompactTxId :: Property
roundTripCompactTxId =
  eachOf 1000 genTxId (trippingCompact toCompactTxId fromCompactTxId)

prop_heapWordsSavingsCompactTxId :: Property
prop_heapWordsSavingsCompactTxId = withTests 1000 $ property $ do
  txId <- forAll genTxId
  let compactTxId = toCompactTxId txId
  assert $ heapWords compactTxId < heapWords txId

--------------------------------------------------------------------------------
-- Compact TxOut
--------------------------------------------------------------------------------

roundTripCompactTxOut :: Property
roundTripCompactTxOut =
  eachOf 1000 genTxOut (trippingCompact toCompactTxOut fromCompactTxOut)

prop_heapWordsSavingsCompactTxOut :: Property
prop_heapWordsSavingsCompactTxOut = withTests 1000 $ property $ do
  txOut <- forAll genTxOut
  let compactTxOut = toCompactTxOut txOut
  assert $ heapWords compactTxOut < heapWords txOut

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
