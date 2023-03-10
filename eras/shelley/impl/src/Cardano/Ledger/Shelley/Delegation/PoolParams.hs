module Cardano.Ledger.Shelley.Delegation.PoolParams (
  poolSpec,
)
where

import Cardano.Ledger.BaseTypes (UnitInterval)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))

poolSpec :: PoolParams c -> (Coin, UnitInterval, Coin)
poolSpec pool = (ppCost pool, ppMargin pool, ppPledge pool)
{-# DEPRECATED poolSpec "Use accessor functions directly `ppCost`, `ppMargin` and `ppPledge`" #-}
