{-# LANGUAGE OverloadedStrings #-}
module Ledger.Core.Generators.Properties
  (relevantKValuesAreGenerated)
where

import           Control.Monad (when)
import           Data.Word (Word64)
import           Hedgehog (Property, cover, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen

import qualified Ledger.Core.Generators as CoreGen
import qualified Ledger.GlobalParams as GP


-- | Coverage test to check that we're generating relevant 'k' values.
relevantKValuesAreGenerated :: Property
relevantKValuesAreGenerated = withTests 300 $ property $ do
  -- chainLength <- forAll $ Gen.element [ 0 :: Word64
  --                                     , 10
  --                                     , 100
  --                                     , 500
  --                                     , 1000
  --                                     ]
  let chainLength = 1000 :: Word64
  k <- forAll $ CoreGen.k chainLength (chainLength `div` 10)
  cover 10
    "k == 0"
    (k == 0)
  cover 70
    "k /= 0"
    (k /= 0)
  let slotsPerEpoch :: Word64
      slotsPerEpoch = GP.slotsPerEpoch k
  when (slotsPerEpoch /= 0) $ do
    let
      epochs :: Double
      epochs = fromIntegral chainLength / fromIntegral slotsPerEpoch
    cover 20
      "1 epochs "
      (epochs == 1)

    cover 20
      "[2, 10)"
      (2 <= epochs && epochs < 10)

    cover 20
      "[10, 20)"
      (10 <= epochs && epochs < 20)

    cover 20
      "[20, 30)"
      (20 <= epochs && epochs < 30)

    cover 20
      "[30, 40)"
      (30 <= epochs && epochs < 40)

    cover 20
      "[40, 50)"
      (40 <= epochs && epochs < 50)

    cover 20
      "[50, 100)"
      (50 <= epochs && epochs < 100)
