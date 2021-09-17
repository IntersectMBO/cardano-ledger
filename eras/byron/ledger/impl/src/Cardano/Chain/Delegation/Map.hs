{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Chain.Delegation.Map
  ( Map (..),

    -- * Query
    memberR,
    notMemberR,
    pairMember,
    lookupR,

    -- * Update
    insert,

    -- * Conversion/traversal
    fromList,
    keysSet,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Chain.Common.KeyHash (KeyHash)
import Cardano.Prelude hiding (Map)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import NoThunks.Class (NoThunks (..), noThunksInKeysAndValues)

newtype Map = Map
  { unMap :: Bimap KeyHash KeyHash
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData)

instance FromCBOR Map where
  fromCBOR = Map . Bimap.fromList <$> fromCBOR

instance ToCBOR Map where
  toCBOR = toCBOR . Bimap.toList . unMap

-- | A 'Bimap' contains two regular 'Map's, which are spine strict; we therefore
-- have to worry about the elements only
instance NoThunks Map where
  wNoThunks ctxt =
    noThunksInKeysAndValues ctxt
      . Bimap.toList
      . unMap

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

memberR :: KeyHash -> Map -> Bool
memberR b = Bimap.memberR b . unMap

-- TODO: maybe we should call these @delegate@ and @notADelegate@ (and add also a @delegator@) function.

notMemberR :: KeyHash -> Map -> Bool
notMemberR b = Bimap.notMemberR b . unMap

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
