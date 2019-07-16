{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Delegation.PoolParams
  ( poolSpec
  ) where

import           Lens.Micro    ((^.))

import           BaseTypes
import           Coin          (Coin)
import           TxData

poolSpec :: PoolParams hashAlgo dsignAlgo -> (Coin, UnitInterval, Coin)
poolSpec pool = (pool ^. poolCost, pool ^. poolMargin, pool ^. poolPledge)
