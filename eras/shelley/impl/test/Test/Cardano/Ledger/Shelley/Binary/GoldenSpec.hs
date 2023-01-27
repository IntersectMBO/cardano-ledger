{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.GoldenSpec (spec) where

import Cardano.Ledger.Shelley
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.Golden (goldenNewEpochStateExpectation)

spec :: Spec
spec =
  describe "Golden" $ do
    prop "NewEpochState" $ goldenNewEpochStateExpectation @Shelley
