{-# LANGUAGE ImportQualifiedPost #-}

module Test.Cardano.Ledger.Conformance.Spec.Dijkstra (spec) where

import Test.Cardano.Ledger.Conformance.Spec.Dijkstra.Rewards qualified as Rewards
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = do
  describe "Spec" $ do
    Rewards.spec
