{-# LANGUAGE TemplateHaskell   #-}

module Delegation.StakePool
  ( StakePool(..)
  , Delegation(..)
  -- lenses
    -- StakePool
  , poolPubKey
  , poolPledge
  , poolPledges
  , poolCost
  , poolMargin
  , poolAltAcnt
  , poolSpec
    -- Delegation
  , delegator
  , delegatee
  ) where

import           Data.Map        (Map)

import           Lens.Micro.TH   (makeLenses)
import           Lens.Micro      ((^.))

import           Coin            (Coin)
import           Keys
import           PParams

-- |A stake pool.
data StakePool = StakePool
                   { _poolPubKey  :: VKey
                   , _poolPledge  :: Coin
                   , _poolPledges :: Map VKey Coin -- TODO not updated currently
                   , _poolCost    :: Coin
                   , _poolMargin  :: UnitInterval
                   , _poolAltAcnt :: Maybe HashKey
                   } deriving (Show, Eq, Ord)

makeLenses ''StakePool

poolSpec :: StakePool -> (Coin, UnitInterval, Coin)
poolSpec pool = (pool ^. poolCost, pool ^. poolMargin, pool ^. poolPledge)

-- |The delegation of one stake key to another.
data Delegation = Delegation { _delegator :: VKey
                             , _delegatee :: VKey }
                             deriving (Show, Eq, Ord)

makeLenses ''Delegation
