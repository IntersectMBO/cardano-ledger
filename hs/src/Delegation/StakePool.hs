module Delegation.StakePool
  ( StakePool(..)
  , Delegation(..)
  ) where

import           Data.Map        (Map)
import           Data.Ratio
import           Numeric.Natural

import           Coin            (Coin)
import           Keys

-- |A stake pool.
data StakePool = StakePool
                   { poolPubKey  :: VKey
                   , poolPledges :: Map VKey Coin -- TODO not updated currently
                   , poolCost    :: Coin
                   , poolMargin  :: Ratio Natural -- TODO is float okay?
                                                  -- how about positive rationals?
                   , poolAltAcnt :: Maybe HashKey
                   } deriving (Show, Eq, Ord)

-- TODO: Does this need the sum of unspent UTxO for the delegator?
-- |The delegation of one stake key to another.
data Delegation = Delegation { delegator :: VKey
                             , delegatee :: VKey }
                             deriving (Show, Eq, Ord)
