{-# LANGUAGE OverloadedStrings #-}

module Test.Byron.Spec.Ledger.Core.Generators.Properties (relevantKValuesAreGenerated) where

import qualified Byron.Spec.Ledger.Core.Generators as CoreGen
import qualified Byron.Spec.Ledger.GlobalParams as GP
import Control.Monad (when)
import Data.Word (Word64)
import Hedgehog (Property, cover, forAll, property, withTests)

-- | Coverage test to check that we're generating relevant 'k' values.
relevantKValuesAreGenerated :: Property
relevantKValuesAreGenerated = withTests 500 $
  property $ do
    let chainLength = 1000 :: Word64

    k <- forAll $ CoreGen.k chainLength (chainLength `div` 10)

    let slotsPerEpoch :: Word64
        slotsPerEpoch = GP.slotsPerEpoch k

    when (slotsPerEpoch /= 0) $ do
      let epochs :: Word64
          epochs = round $ fromIntegral chainLength / (fromIntegral slotsPerEpoch :: Double)

      cover
        5
        "1 epochs "
        (epochs == 1)

      cover
        20
        "epochs in [2, 25)"
        (2 <= epochs && epochs < 25)

      cover
        5
        "epochs in [25, 50)"
        (25 <= epochs && epochs < 50)

      cover
        5
        "50 epochs "
        (epochs == 50)

      -- Note that we will not get any epochs between 50 and 100 since this will require the value of
      -- @k@ to be a fraction. For instance, to get a @k@ value that will produce 70 epochs for a
      -- chain of length 1000, we need (assuming @10k@ slots per-epoch):
      --
      -- > 1000 / (10 * 70) ~ 1.428
      --
      -- So if we round this value up, we get 50 epochs:
      --
      -- > 1000 / (10 * 2) == 50
      --
      -- And if we round this value down we get 100 epochs.

      cover
        6
        "100 epochs "
        (epochs == 100)
