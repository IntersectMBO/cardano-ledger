{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Annotator (
  module Test.Cardano.Ledger.Conway.Binary.Annotator,
) where

import Cardano.Ledger.Binary (Annotator, DecCBOR (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxBody (DijkstraTxBodyRaw, TxBody (MkDijkstraTxBody))
import Test.Cardano.Ledger.Conway.Binary.Annotator (Mem)

instance DecCBOR (Annotator DijkstraTxBodyRaw) where
  decCBOR = pure <$> decCBOR

deriving via Mem DijkstraTxBodyRaw instance DecCBOR (Annotator (TxBody DijkstraEra))
