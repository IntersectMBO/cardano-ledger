{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Chain.Delegation.Map
  ( Map(..)

  -- * Query
  , memberR
  , pairMember
  , lookupR

  -- * Update
  , insert

  -- * Conversion/traversal
  , fromList
  , keysSet
  )
where

import Cardano.Prelude hiding (Map)

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set

import Cardano.Chain.Common.StakeholderId (StakeholderId)


newtype Map = Map
  { unMap :: Bimap StakeholderId StakeholderId
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData


--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

memberR :: StakeholderId -> Map -> Bool
memberR b = Bimap.memberR b . unMap

pairMember :: (StakeholderId, StakeholderId) -> Map -> Bool
pairMember p = Bimap.pairMember p . unMap

lookupR :: StakeholderId -> Map -> Maybe StakeholderId
lookupR b = Bimap.lookupR b . unMap


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

insert :: StakeholderId -> StakeholderId -> Map -> Map
insert a b = Map . Bimap.insert a b . unMap


--------------------------------------------------------------------------------
-- Conversion/traversal
--------------------------------------------------------------------------------

fromList :: [(StakeholderId, StakeholderId)] -> Map
fromList = Map . Bimap.fromList

keysSet :: Map -> Set StakeholderId
keysSet = Set.fromList . Bimap.keys . unMap
