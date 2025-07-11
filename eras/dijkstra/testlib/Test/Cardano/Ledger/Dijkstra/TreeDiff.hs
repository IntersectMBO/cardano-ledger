{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.TreeDiff (
  module Test.Cardano.Ledger.Conway.TreeDiff,
) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (EraTx (..), EraTxBody (..), PlutusScript)
import Cardano.Ledger.Dijkstra.TxBody (DijkstraTxBodyRaw)
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)

instance ToExpr (PlutusScript DijkstraEra)

instance ToExpr DijkstraTxBodyRaw

instance ToExpr (TxBody DijkstraEra)

instance ToExpr (Tx DijkstraEra)
