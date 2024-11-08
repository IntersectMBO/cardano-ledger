{-# LANGUAGE TypeApplications #-}

module Test.Suite.ImpSpec (spec) where

import Test.ImpSpec

data I

instance ImpSpec I

spec :: Spec
spec =
  describe "ImpSpec" $ do
    describe "Expectations" $ do
      it "shouldBeLeft" $ io $ do
        Left @() @Int () `shouldBeLeft` ()
    withImpInit @I $
      describe "ImpM" $ do
        it "impSetSeed" $ do
          impSetSeed 1234
          arbitrary `shouldReturn` 'F'
