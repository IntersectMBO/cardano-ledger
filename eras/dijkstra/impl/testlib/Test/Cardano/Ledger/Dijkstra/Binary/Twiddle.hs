{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Twiddle (
  module Test.Cardano.Ledger.Conway.Binary.Twiddle,
) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxCert (DijkstraTxCert)
import Cardano.Ledger.Val (Val)
import Test.Cardano.Ledger.Conway.Binary.Twiddle

instance (Era era, Val (Value era)) => Twiddle (DijkstraTxCert era) where
  twiddle = twiddleTerm

instance Twiddle (Tx DijkstraEra) where
  twiddle = twiddleTerm

instance Twiddle (TxBody DijkstraEra) where
  twiddle = twiddleTerm

instance Twiddle (PParams DijkstraEra) where
  twiddle = twiddleTerm

instance Twiddle (PParamsUpdate DijkstraEra) where
  twiddle = twiddleTerm
