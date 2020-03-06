module Shelley.Spec.Ledger.Delegation.PoolParams
  ( poolSpec
  ) where

import           Lens.Micro ((^.))

import           Shelley.Spec.Ledger.BaseTypes (UnitInterval)
import           Shelley.Spec.Ledger.Coin (Coin)
import           Shelley.Spec.Ledger.TxData (PoolParams, poolCost, poolMargin, poolPledge)

poolSpec :: PoolParams crypto -> (Coin, UnitInterval, Coin)
poolSpec pool = (pool ^. poolCost, pool ^. poolMargin, pool ^. poolPledge)
