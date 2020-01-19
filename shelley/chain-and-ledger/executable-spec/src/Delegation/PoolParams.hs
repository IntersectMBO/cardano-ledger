module Delegation.PoolParams
  ( poolSpec
  ) where

import           Lens.Micro ((^.))

import           BaseTypes (UnitInterval)
import           TxData (PoolParams, poolCost, poolMargin, poolPledge)
import           Scripts

poolSpec :: PoolParams crypto -> (Value crypto, UnitInterval, Value crypto)
poolSpec pool = (pool ^. poolCost, pool ^. poolMargin, pool ^. poolPledge)
