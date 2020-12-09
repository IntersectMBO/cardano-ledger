module Shelley.Spec.Ledger.Delegation.PoolParams
  ( poolSpec,
  )
where

import Shelley.Spec.Ledger.BaseTypes (UnitInterval)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.TxBody (PoolParams (..))

poolSpec :: PoolParams crypto -> (Coin, UnitInterval, Coin)
poolSpec pool = (_poolCost pool, _poolMargin pool, _poolPledge pool)
