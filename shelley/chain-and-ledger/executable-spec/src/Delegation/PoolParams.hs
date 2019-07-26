module Delegation.PoolParams
  ( poolSpec
  ) where

import           Lens.Micro ((^.))

import           BaseTypes (UnitInterval)
import           Coin (Coin)
import           TxData (PoolParams, poolCost, poolMargin, poolPledge)

poolSpec :: PoolParams hashAlgo dsignAlgo -> (Coin, UnitInterval, Coin)
poolSpec pool = (pool ^. poolCost, pool ^. poolMargin, pool ^. poolPledge)
