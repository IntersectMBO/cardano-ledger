{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Arbitrary () where

import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (DijkstraPParams, UpgradeDijkstraPParams)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraNativeScript, DijkstraPlutusPurpose)
import Cardano.Ledger.Dijkstra.Transition (TransitionConfig (..))
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Cardano.Ledger.Dijkstra.TxCert
import Data.Functor.Identity (Identity)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Allegra.Arbitrary (maxTimelockDepth, sizedTimelock)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()

instance Arbitrary (DijkstraPParams Identity DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraPParams StrictMaybe DijkstraEra) where
  arbitrary = genericArbitraryU

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

instance Arbitrary (UpgradeDijkstraPParams Identity DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary DijkstraGenesis where
  arbitrary = genericArbitraryU

instance Arbitrary (TransitionConfig DijkstraEra) where
  arbitrary = DijkstraTransitionConfig <$> arbitrary <*> arbitrary

instance
  (forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b)) =>
  Arbitrary (DijkstraPlutusPurpose f DijkstraEra)
  where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraNativeScript DijkstraEra) where
  arbitrary = sizedTimelock maxTimelockDepth

deriving newtype instance Arbitrary (Tx DijkstraEra)

instance Era era => Arbitrary (DijkstraTxCert era) where
  arbitrary =
    oneof
      [ DijkstraTxCertDeleg <$> arbitrary
      , DijkstraTxCertPool <$> arbitrary
      , DijkstraTxCertGov <$> arbitrary
      ]

instance Arbitrary DijkstraDelegCert where
  arbitrary = DijkstraRegDelegCert <$> arbitrary <*> arbitrary <*> arbitrary
