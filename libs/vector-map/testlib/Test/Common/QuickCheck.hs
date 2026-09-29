{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Common.QuickCheck (
  testPropertyN,
  withMaxTimesSuccess,
  testLawsGroup,
) where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (Proxy))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable)
import Test.QuickCheck.Classes.Base (Laws (Laws, lawsProperties, lawsTypeclass))
import Test.QuickCheck.Property (Result (maybeNumTests), mapTotalResult)

testPropertyN :: Testable prop => Int -> String -> prop -> Spec
testPropertyN n name = prop name . withMaxTimesSuccess n

withMaxTimesSuccess :: Testable prop => Int -> prop -> Property
withMaxTimesSuccess !n =
  mapTotalResult $ \res -> res {maybeNumTests = (n *) <$> (maybeNumTests res <|> Just 100)}

-- | Check the typeclass `Laws` of a type, one example per law:
--
-- > describe "Semigroup and Monoid" $
-- >   testLawsGroup @ValidityInterval
-- >     [ semigroupLaws
-- >     , monoidLaws
-- >     ]
--
-- This should be used instead of `Test.QuickCheck.Classes.lawsCheckOne`, which
-- reports through `Test.QuickCheck.quickCheck`. That writes straight to stdout,
-- thus it is not probably indented in the test console output. It also discards
-- the QuickCheck `Result`, which means that a violated law does not fail the
-- test suite.
testLawsGroup :: forall a. [Proxy a -> Laws] -> Spec
testLawsGroup =
  traverse_ $ \mkLaws -> do
    let Laws {..} = mkLaws (Proxy @a)
    describe lawsTypeclass $ traverse_ (uncurry prop) lawsProperties
