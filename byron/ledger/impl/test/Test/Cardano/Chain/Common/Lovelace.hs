{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Common.Lovelace
  ( tests,
  )
where

import Cardano.Chain.Common
  ( LovelaceError (..),
    addLovelace,
    integerToLovelace,
    maxLovelaceVal,
    mkKnownLovelace,
    mkLovelace,
    scaleLovelace,
    subLovelace,
    unsafeGetLovelace,
  )
import Cardano.Prelude
import Data.Data (Constr, toConstr)
import Formatting (build, sformat)
import Hedgehog (Property, discover, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Chain.Common.Gen (genCustomLovelace, genLovelace)
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, withTestsTS)

ts_prop_addLovelace :: TSProperty
ts_prop_addLovelace = withTestsTS 1000 . property $ do
  a <- forAll genLovelace
  let newRange = maxLovelaceVal - unsafeGetLovelace a
  b <- forAll $ genCustomLovelace newRange
  assertIsRight $ addLovelace a b

prop_addLovelaceOverflow :: Property
prop_addLovelaceOverflow =
  property $
    assertIsLeftConstr
      dummyLovelaceOverflow
      (addLovelace (mkKnownLovelace @1) maxBound)

ts_prop_integerToLovelace :: TSProperty
ts_prop_integerToLovelace = withTestsTS 1000 . property $ do
  testInt <-
    forAll
      (Gen.integral $ Range.linear 0 (fromIntegral maxLovelaceVal :: Integer))
  assertIsRight $ integerToLovelace testInt

prop_integerToLovelaceTooLarge :: Property
prop_integerToLovelaceTooLarge =
  property $
    assertIsLeftConstr
      dummyLovelaceTooLarge
      (integerToLovelace (fromIntegral (maxLovelaceVal + 1) :: Integer))

prop_integerToLovelaceTooSmall :: Property
prop_integerToLovelaceTooSmall =
  property $
    assertIsLeftConstr dummyLovelaceTooSmall (integerToLovelace (negate 1))

prop_maxLovelaceUnchanged :: Property
prop_maxLovelaceUnchanged =
  property $ (fromIntegral maxLovelaceVal :: Integer) === 45e15

ts_prop_mkLovelace :: TSProperty
ts_prop_mkLovelace = withTestsTS 1000 . property $ do
  testWrd <- forAll (Gen.word64 $ Range.linear 0 maxLovelaceVal)
  assertIsRight $ mkLovelace testWrd

prop_mkLovelaceTooLarge :: Property
prop_mkLovelaceTooLarge =
  property $
    assertIsLeftConstr dummyLovelaceTooLarge (mkLovelace (maxLovelaceVal + 1))

prop_scaleLovelaceTooLarge :: Property
prop_scaleLovelaceTooLarge =
  property $
    assertIsLeftConstr
      dummyLovelaceTooLarge
      (scaleLovelace maxBound (2 :: Integer))

ts_prop_subLovelace :: TSProperty
ts_prop_subLovelace = withTestsTS 1000 . property $ do
  a <- forAll genLovelace
  b <- forAll $ genCustomLovelace (unsafeGetLovelace a)
  assertIsRight $ subLovelace a b

ts_prop_subLovelaceUnderflow :: TSProperty
ts_prop_subLovelaceUnderflow =
  withTestsTS 1000
    . property
    $ do
      -- (maxLovelaveVal - 1) to avoid an overflow error in `addLovelace`
      -- in case expression
      a <- forAll $ genCustomLovelace (maxLovelaceVal - 1)
      case addLovelace a (mkKnownLovelace @1) of
        Right added ->
          assertIsLeftConstr dummyLovelaceUnderflow (subLovelace a added)
        Left err ->
          panic $
            sformat
              ("The impossible happened in subLovelaceUnderflow: " . build)
              err

tests :: TSGroup
tests = concatTSGroups [const $$discover, $$discoverPropArg]

--------------------------------------------------------------------------------
-- Dummy values for constructor comparison in assertIsLeftConstr tests
--------------------------------------------------------------------------------

dummyLovelaceOverflow :: Constr
dummyLovelaceOverflow = toConstr $ LovelaceOverflow 1

dummyLovelaceTooLarge :: Constr
dummyLovelaceTooLarge = toConstr $ LovelaceTooLarge 1

dummyLovelaceTooSmall :: Constr
dummyLovelaceTooSmall = toConstr $ LovelaceTooSmall 1

dummyLovelaceUnderflow :: Constr
dummyLovelaceUnderflow = toConstr $ LovelaceUnderflow 1 1
