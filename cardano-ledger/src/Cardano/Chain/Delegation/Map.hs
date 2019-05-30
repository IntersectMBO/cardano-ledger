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

import Cardano.Chain.Common.KeyHash (KeyHash)


newtype Map = Map
  { unMap :: Bimap KeyHash KeyHash
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData


--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

memberR :: KeyHash -> Map -> Bool
memberR b = Bimap.memberR b . unMap

pairMember :: (KeyHash, KeyHash) -> Map -> Bool
pairMember p = Bimap.pairMember p . unMap

lookupR :: KeyHash -> Map -> Maybe KeyHash
lookupR b = Bimap.lookupR b . unMap


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

insert :: KeyHash -> KeyHash -> Map -> Map
insert a b = Map . Bimap.insert a b . unMap


--------------------------------------------------------------------------------
-- Conversion/traversal
--------------------------------------------------------------------------------

fromList :: [(KeyHash, KeyHash)] -> Map
fromList = Map . Bimap.fromList

keysSet :: Map -> Set KeyHash
keysSet = Set.fromList . Bimap.keys . unMap
