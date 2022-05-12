{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Compact.Common
  ( testLawsGroup,
    testPropertyN,
    withMaxTimesSuccess,
    module X,
  )
where

import Control.Applicative
import Data.Proxy as X
import Test.QuickCheck.Classes.Base as X
import Test.QuickCheck.Property (mapTotalResult, maybeNumTests)
import Test.Tasty as X
import Test.Tasty.QuickCheck as X

withMaxTimesSuccess :: Testable prop => Int -> prop -> Property
withMaxTimesSuccess !n =
  mapTotalResult $ \res -> res {maybeNumTests = (n *) <$> (maybeNumTests res <|> Just 100)}

testPropertyN :: Testable prop => Int -> TestName -> prop -> TestTree
testPropertyN n name = testProperty name . withMaxTimesSuccess n

testLawsGroup :: TestName -> [Laws] -> TestTree
testLawsGroup name = testGroup name . fmap testLaws
  where
    testLaws Laws {..} =
      testGroup lawsTypeclass $ fmap (uncurry testProperty) lawsProperties
