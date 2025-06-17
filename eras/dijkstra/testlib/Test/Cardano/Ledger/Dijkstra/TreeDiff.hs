{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.TreeDiff (
  module Test.Cardano.Ledger.Conway.TreeDiff,
) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (EraTxBody (..), PlutusScript)
import Cardano.Ledger.Dijkstra.TxBody (DijkstraTxBodyRaw)
import Cardano.Ledger.Dijkstra.TxCert
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)

instance ToExpr (PlutusScript DijkstraEra)

instance ToExpr DijkstraTxBodyRaw

instance ToExpr (TxBody DijkstraEra)

instance ToExpr DijkstraDelegCert

instance ToExpr (DijkstraTxCert era)
