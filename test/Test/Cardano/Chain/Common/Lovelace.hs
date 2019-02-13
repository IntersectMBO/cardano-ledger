{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Common.Lovelace
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Formatting (build, sformat)

import Hedgehog
  (Property, property, discover, (===), withTests, forAll, property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Common
  ( addLovelace
  , integerToLovelace
  , maxLovelaceVal
  , mkKnownLovelace
  , mkLovelace
  , scaleLovelace
  , subLovelace
  , unsafeGetLovelace
  )

import Test.Cardano.Chain.Common.Gen (genLovelace, genCustomLovelace)

prop_addLovelace :: Property
prop_addLovelace = withTests 1000 . property $ do
  a <- forAll genLovelace
  let newRange = maxLovelaceVal - unsafeGetLovelace a
  b <- forAll $ genCustomLovelace newRange
  assertEitherIsRight (addLovelace a) b

prop_addLovelaceOverflow :: Property
prop_addLovelaceOverflow =
  property $ assertEitherIsLeft (addLovelace (mkKnownLovelace @1)) maxBound

prop_integerToLovelace :: Property
prop_integerToLovelace = eachOf
  1000
  (Gen.integral $ Range.linear 0 (fromIntegral maxLovelaceVal :: Integer))
  (assertEitherIsRight integerToLovelace)

prop_integerToLovelaceTooLarge :: Property
prop_integerToLovelaceTooLarge =
  property $ (assertEitherIsLeft integerToLovelace)
    (fromIntegral (maxLovelaceVal + 1) :: Integer)

prop_integerToLovelaceTooSmall :: Property
prop_integerToLovelaceTooSmall =
  property $ (assertEitherIsLeft integerToLovelace) (negate 1)

prop_maxLovelaceUnchanged :: Property
prop_maxLovelaceUnchanged =
  property $ (fromIntegral maxLovelaceVal :: Integer) === 45e15

prop_mkLovelace :: Property
prop_mkLovelace = eachOf
  1000
  (Gen.word64 $ Range.linear 0 maxLovelaceVal)
  (assertEitherIsRight mkLovelace)

prop_mkLovelaceTooLarge :: Property
prop_mkLovelaceTooLarge =
  property $ (assertEitherIsLeft mkLovelace) (maxLovelaceVal + 1)

prop_scaleLovelaceTooLarge :: Property
prop_scaleLovelaceTooLarge =
  property $ (assertEitherIsLeft $ scaleLovelace maxBound) (2 :: Integer)

prop_subLovelace :: Property
prop_subLovelace = withTests 1000 . property $ do
  a <- forAll genLovelace
  b <- forAll $ genCustomLovelace (unsafeGetLovelace a)
  assertEitherIsRight (subLovelace a) b

prop_subLovelaceUnderflow :: Property
prop_subLovelaceUnderflow =
  withTests 1000
    . property
    $ do
        -- (maxLovelaveVal - 1) to avoid an overflow error in `addLovelace`
        -- in case expression
        a <- forAll $ genCustomLovelace (maxLovelaceVal - 1)
        case addLovelace a (mkKnownLovelace @1) of
          Right added -> assertEitherIsLeft (subLovelace a) added
          Left  err   -> panic $ sformat
            ("The impossible happened in subLovelaceUnderflow: " . build)
            err

tests :: IO Bool
tests = H.checkParallel $$(discover)
