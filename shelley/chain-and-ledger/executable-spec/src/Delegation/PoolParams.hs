{-# LANGUAGE TemplateHaskell #-}

module Delegation.PoolParams
  ( PoolParams(..)
  , Delegation(..)
  , RewardAcnt(..)
  -- lenses
  , poolPubKey
  , poolPledge
  , poolPledges
  , poolCost
  , poolMargin
  , poolAltAcnt
  , poolSpec
  , poolRAcnt
  , poolOwners
    -- Delegation
  , delegator
  , delegatee
  ) where

import           Data.Map.Strict (Map)
import           Data.Set      (Set)

import           Lens.Micro    ((^.))
import           Lens.Micro.TH (makeLenses)

import           BaseTypes
import           Coin          (Coin)
import           Keys

-- |An account based address for a rewards
newtype RewardAcnt = RewardAcnt
  { getRwdHK :: HashKey
  } deriving (Show, Eq, Ord)

-- |A stake pool.
data PoolParams = PoolParams
  { _poolPubKey  :: VKey
  , _poolPledge  :: Coin
  , _poolPledges :: Map VKey Coin -- TODO not updated currently
  , _poolCost    :: Coin
  , _poolMargin  :: UnitInterval
  , _poolAltAcnt :: Maybe HashKey
  , _poolRAcnt   :: RewardAcnt
  , _poolOwners  :: Set HashKey
  } deriving (Show, Eq, Ord)

makeLenses ''PoolParams

poolSpec :: PoolParams -> (Coin, UnitInterval, Coin)
poolSpec pool = (pool ^. poolCost, pool ^. poolMargin, pool ^. poolPledge)

-- |The delegation of one stake key to another.
data Delegation = Delegation
  { _delegator :: VKey
  , _delegatee :: VKey
  } deriving (Show, Eq, Ord)

makeLenses ''Delegation
