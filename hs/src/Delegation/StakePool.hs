module Delegation.StakePool
  ( StakePool(..)
  , Delegation(..)
  ) where

import           Data.Map        (Map)
import           Data.Ratio
import           Numeric.Natural

import           Lovelace        (Lovelace)
import           Keys

-- |A stake pool.
data StakePool = StakePool
                   { poolPubKey  :: VKey
                   , poolPledges :: Map VKey Lovelace -- TODO not updated currently
                   , poolCost    :: Lovelace
                   , poolMargin  :: Ratio Natural
                   , poolAltAcnt :: Maybe HashKey
                   } deriving (Show, Eq, Ord)

-- |The delegation of one stake key to another.
data Delegation = Delegation { delegator :: VKey
                             , delegatee :: VKey }
                             deriving (Show, Eq, Ord)
