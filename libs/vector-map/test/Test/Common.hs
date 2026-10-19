{-# LANGUAGE BangPatterns #-}

module Test.Common (
  testPropertyN,
  withMaxTimesSuccess,
) where

import Control.Applicative ((<|>))
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable)
import Test.QuickCheck.Property (Result (maybeNumTests), mapTotalResult)

withMaxTimesSuccess :: Testable prop => Int -> prop -> Property
withMaxTimesSuccess !n =
  mapTotalResult $ \res -> res {maybeNumTests = (n *) <$> (maybeNumTests res <|> Just 100)}

testPropertyN :: Testable prop => Int -> String -> prop -> Spec
testPropertyN n name = prop name . withMaxTimesSuccess n
