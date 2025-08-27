{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.TreeDiff (
  module Test.Cardano.Ledger.Conway.TreeDiff,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (EraTx (..), EraTxBody (..), PlutusScript)
import Cardano.Ledger.Dijkstra.PParams (DijkstraPParams)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose)
import Cardano.Ledger.Dijkstra.TxBody (DijkstraTxBodyRaw)
import Data.Functor.Identity (Identity)
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)

instance
  (forall a b. (ToExpr a, ToExpr b) => ToExpr (f a b)) =>
  ToExpr (DijkstraPlutusPurpose f DijkstraEra)

instance ToExpr (PlutusScript DijkstraEra)

instance ToExpr (DijkstraPParams Identity DijkstraEra)

instance ToExpr (DijkstraPParams StrictMaybe DijkstraEra)

instance ToExpr DijkstraTxBodyRaw

instance ToExpr (TxBody DijkstraEra)

instance ToExpr (Tx DijkstraEra)
