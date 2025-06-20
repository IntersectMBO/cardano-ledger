{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Cardano.Ledger.Dijkstra.Arbitrary () where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.Transition (TransitionConfig (..))
import Cardano.Ledger.Dijkstra.Core (EraTxBody (..), EraTx (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Test.Cardano.Ledger.Common (Arbitrary (..), scale)
import Test.Cardano.Ledger.Conway.Arbitrary ()

instance Arbitrary (TxBody DijkstraEra) where
  arbitrary =
    DijkstraTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary DijkstraGenesis where
  arbitrary = pure DijkstraGenesis

instance Arbitrary (TransitionConfig DijkstraEra) where
  arbitrary = DijkstraTransitionConfig <$> arbitrary <*> arbitrary

deriving newtype instance Arbitrary (Tx DijkstraEra)
