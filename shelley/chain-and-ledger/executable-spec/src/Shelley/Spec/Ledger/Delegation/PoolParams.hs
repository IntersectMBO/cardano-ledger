module Shelley.Spec.Ledger.Delegation.PoolParams
  ( poolSpec,
  )
where

import Shelley.Spec.Ledger.BaseTypes (UnitInterval)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.TxData (PoolParams (..))

poolSpec :: PoolParams era -> (Coin, UnitInterval, Coin)
poolSpec pool = (_poolCost pool, _poolMargin pool, _poolPledge pool)
