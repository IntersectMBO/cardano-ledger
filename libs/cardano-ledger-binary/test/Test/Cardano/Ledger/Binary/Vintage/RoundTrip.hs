{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Vintage.RoundTrip (tests) where

import Data.Fixed (E9, Fixed (..))
import Data.Ratio ((%))
import Hedgehog (Property, Range, checkParallel)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Ledger.Binary.Vintage.Helpers.GoldenRoundTrip (
  roundTripsCBORBuildable,
  roundTripsCBORShow,
 )
import Test.Cardano.Prelude (discoverRoundTrip, eachOf)

tests :: IO Bool
tests = checkParallel $$discoverRoundTrip

roundTripUnitBi :: Property
roundTripUnitBi = eachOf 1 (pure ()) roundTripsCBORBuildable

roundTripBoolBi :: Property
roundTripBoolBi = eachOf 10 Gen.bool roundTripsCBORBuildable

-- | Tests up to 'Integer's with multiple machine words using large upper bound
roundTripIntegerBi :: Property
roundTripIntegerBi =
  eachOf
    1000
    (Gen.integral (Range.linearFrom 0 (-1e40) 1e40 :: Range Integer))
    roundTripsCBORBuildable

roundTripWordBi :: Property
roundTripWordBi =
  eachOf 1000 (Gen.word Range.constantBounded) roundTripsCBORBuildable

roundTripWord8Bi :: Property
roundTripWord8Bi =
  eachOf 1000 (Gen.word8 Range.constantBounded) roundTripsCBORBuildable

roundTripWord16Bi :: Property
roundTripWord16Bi =
  eachOf 1000 (Gen.word16 Range.constantBounded) roundTripsCBORBuildable

roundTripWord32Bi :: Property
roundTripWord32Bi =
  eachOf 1000 (Gen.word32 Range.constantBounded) roundTripsCBORBuildable

roundTripWord64Bi :: Property
roundTripWord64Bi =
  eachOf 1000 (Gen.word64 Range.constantBounded) roundTripsCBORBuildable

roundTripIntBi :: Property
roundTripIntBi =
  eachOf 1000 (Gen.int Range.constantBounded) roundTripsCBORBuildable

roundTripFloatBi :: Property
roundTripFloatBi =
  eachOf 1000 (Gen.float (Range.constant (-1e12) 1e12)) roundTripsCBORBuildable

roundTripInt32Bi :: Property
roundTripInt32Bi =
  eachOf 1000 (Gen.int32 Range.constantBounded) roundTripsCBORBuildable

roundTripInt64Bi :: Property
roundTripInt64Bi =
  eachOf 1000 (Gen.int64 Range.constantBounded) roundTripsCBORBuildable

roundTripRatioBi :: Property
roundTripRatioBi =
  let r :: Range.Range Integer
      r = Range.constant (-1_000_000_000_000_0000_000) 1_000_000_000_000_0000_000
   in eachOf
        1000
        ((%) <$> Gen.integral r <*> Gen.integral r)
        roundTripsCBORBuildable

roundTripNanoBi :: Property
roundTripNanoBi =
  eachOf
    1000
    ((MkFixed :: Integer -> Fixed E9) <$> Gen.integral (Range.constantFrom 0 (-1e12) 1e12))
    roundTripsCBORShow

roundTripMapBi :: Property
roundTripMapBi =
  eachOf
    100
    ( Gen.map
        (Range.constant 0 50)
        ((,) <$> Gen.int Range.constantBounded <*> Gen.int Range.constantBounded)
    )
    roundTripsCBORShow

roundTripSetBi :: Property
roundTripSetBi =
  eachOf
    100
    (Gen.set (Range.constant 0 50) (Gen.int Range.constantBounded))
    roundTripsCBORShow

roundTripByteStringBi :: Property
roundTripByteStringBi =
  eachOf 100 (Gen.bytes $ Range.constant 0 100) roundTripsCBORShow

roundTripTextBi :: Property
roundTripTextBi =
  eachOf
    100
    (Gen.text (Range.constant 0 100) Gen.unicode)
    roundTripsCBORBuildable
