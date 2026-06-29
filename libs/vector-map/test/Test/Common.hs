{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Common (
  testLawsGroup,
  testPropertyN,
  withMaxTimesSuccess,
  module X,
) where

import Control.Applicative
import Data.Foldable (traverse_)
import Data.Proxy as X
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.QuickCheck as X
import Test.QuickCheck.Classes.Base as X
import Test.QuickCheck.Property (mapTotalResult, maybeNumTests)

withMaxTimesSuccess :: Testable prop => Int -> prop -> Property
withMaxTimesSuccess !n =
  mapTotalResult $ \res -> res {maybeNumTests = (n *) <$> (maybeNumTests res <|> Just 100)}

testPropertyN :: Testable prop => Int -> String -> prop -> Spec
testPropertyN n name = prop name . withMaxTimesSuccess n

testLawsGroup :: String -> [Laws] -> Spec
testLawsGroup name = describe name . traverse_ testLaws
  where
    testLaws Laws {..} =
      describe lawsTypeclass $ traverse_ (uncurry prop) lawsProperties
