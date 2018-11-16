{-# LANGUAGE NumDecimals      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Binary.Bi
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Fixed (E9, Fixed(..))
import Hedgehog (Property, Range, checkParallel)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (roundTripsBiBuildable, roundTripsBiShow)


tests :: IO Bool
tests = checkParallel $$discoverRoundTrip

roundTripUnitBi :: Property
roundTripUnitBi = eachOf 1 (pure ()) roundTripsBiBuildable

roundTripBoolBi :: Property
roundTripBoolBi = eachOf 10 Gen.bool roundTripsBiBuildable

roundTripCharBi :: Property
roundTripCharBi = eachOf 1000 Gen.unicode roundTripsBiBuildable

-- | Tests up to 'Integer's with multiple machine words using large upper bound
roundTripIntegerBi :: Property
roundTripIntegerBi = eachOf
  1000
  (Gen.integral (Range.linearFrom 0 (-1e40) 1e40 :: Range Integer))
  roundTripsBiBuildable

roundTripWordBi :: Property
roundTripWordBi =
  eachOf 1000 (Gen.word Range.constantBounded) roundTripsBiBuildable

roundTripWord8Bi :: Property
roundTripWord8Bi =
  eachOf 1000 (Gen.word8 Range.constantBounded) roundTripsBiBuildable

roundTripWord16Bi :: Property
roundTripWord16Bi =
  eachOf 1000 (Gen.word16 Range.constantBounded) roundTripsBiBuildable

roundTripWord32Bi :: Property
roundTripWord32Bi =
  eachOf 1000 (Gen.word32 Range.constantBounded) roundTripsBiBuildable

roundTripWord64Bi :: Property
roundTripWord64Bi =
  eachOf 1000 (Gen.word64 Range.constantBounded) roundTripsBiBuildable

roundTripIntBi :: Property
roundTripIntBi =
  eachOf 1000 (Gen.int Range.constantBounded) roundTripsBiBuildable

roundTripFloatBi :: Property
roundTripFloatBi =
  eachOf 1000 (Gen.float (Range.constant (-1e12) 1e12)) roundTripsBiBuildable

roundTripInt32Bi :: Property
roundTripInt32Bi =
  eachOf 1000 (Gen.int32 Range.constantBounded) roundTripsBiBuildable

roundTripInt64Bi :: Property
roundTripInt64Bi =
  eachOf 1000 (Gen.int64 Range.constantBounded) roundTripsBiBuildable

roundTripNanoBi :: Property
roundTripNanoBi = eachOf
  1000
  (MkFixed @E9 <$> Gen.integral (Range.constantFrom 0 (-1e12) 1e12))
  roundTripsBiShow

roundTripMapBi :: Property
roundTripMapBi = eachOf
  100
  (Gen.map
    (Range.constant 0 50)
    ((,) <$> Gen.int Range.constantBounded <*> Gen.int Range.constantBounded)
  )
  roundTripsBiShow

roundTripSetBi :: Property
roundTripSetBi = eachOf
  100
  (Gen.set (Range.constant 0 50) (Gen.int Range.constantBounded))
  roundTripsBiShow

roundTripByteStringBi :: Property
roundTripByteStringBi =
  eachOf 100 (Gen.bytes $ Range.constant 0 100) roundTripsBiShow

roundTripTextBi :: Property
roundTripTextBi =
  eachOf 100 (Gen.text (Range.constant 0 100) Gen.unicode) roundTripsBiBuildable
