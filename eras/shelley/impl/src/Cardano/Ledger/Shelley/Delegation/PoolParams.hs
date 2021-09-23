module Cardano.Ledger.Shelley.Delegation.PoolParams
  ( poolSpec,
  )
where

import Cardano.Ledger.BaseTypes (UnitInterval)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))

poolSpec :: PoolParams crypto -> (Coin, UnitInterval, Coin)
poolSpec pool = (_poolCost pool, _poolMargin pool, _poolPledge pool)
