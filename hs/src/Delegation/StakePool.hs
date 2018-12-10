{-# LANGUAGE TemplateHaskell   #-}

module Delegation.StakePool
  ( StakePool(..)
  , Delegation(..)
  -- lenses
    -- StakePool
  , poolPubKey
  , poolPledges
  , poolCost
  , poolMargin
  , poolAltAcnt
    -- Delegation
  , delegator
  , delegatee
  ) where

import           Data.Map        (Map)
import           Data.Ratio
import           Numeric.Natural

import Lens.Micro.TH (makeLenses)

import           Coin            (Coin)
import           Keys

-- |A stake pool.
data StakePool = StakePool
                   { _poolPubKey  :: VKey
                   , _poolPledges :: Map VKey Coin -- TODO not updated currently
                   , _poolCost    :: Coin
                   , _poolMargin  :: Ratio Natural
                   , _poolAltAcnt :: Maybe HashKey
                   } deriving (Show, Eq, Ord)

makeLenses ''StakePool

-- |The delegation of one stake key to another.
data Delegation = Delegation { _delegator :: VKey
                             , _delegatee :: VKey }
                             deriving (Show, Eq, Ord)

makeLenses ''Delegation
