{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import           Cardano.Binary (ToCBOR(toCBOR), encodeListLen)

import           BaseTypes
import           Coin          (Coin)
import           Keys

-- |An account based address for a rewards
newtype RewardAcnt hashAlgo signAlgo = RewardAcnt
  { getRwdHK :: HashKey hashAlgo signAlgo
  } deriving (Show, Eq, Ord, ToCBOR)


-- |A stake pool.
data PoolParams hashAlgo dsignAlgo =
  PoolParams
    { _poolPubKey  :: VKey dsignAlgo
    , _poolPledge  :: Coin
    , _poolPledges :: Map (VKey dsignAlgo) Coin -- TODO not updated currently
    , _poolCost    :: Coin
    , _poolMargin  :: UnitInterval
    , _poolAltAcnt :: Maybe (HashKey hashAlgo dsignAlgo)
    , _poolRAcnt   :: RewardAcnt hashAlgo dsignAlgo
    , _poolOwners  :: Set (HashKey hashAlgo dsignAlgo)
    } deriving (Show, Eq, Ord)

makeLenses ''PoolParams

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (PoolParams hashAlgo dsignAlgo)
 where
  toCBOR poolParams =
    encodeListLen 8
      <> toCBOR (_poolPubKey poolParams)
      <> toCBOR (_poolPledge poolParams)
      <> toCBOR (_poolPledges poolParams)
      <> toCBOR (_poolCost poolParams)
      <> toCBOR (_poolMargin poolParams)
      <> toCBOR (_poolAltAcnt poolParams)
      <> toCBOR (_poolRAcnt poolParams)
      <> toCBOR (_poolOwners poolParams)


poolSpec :: PoolParams hashAlgo dsignAlgo -> (Coin, UnitInterval, Coin)
poolSpec pool = (pool ^. poolCost, pool ^. poolMargin, pool ^. poolPledge)

-- |The delegation of one stake key to another.
data Delegation dsignAlgo = Delegation
  { _delegator :: VKey dsignAlgo
  , _delegatee :: VKey dsignAlgo
  } deriving (Show, Eq, Ord)

instance DSIGNAlgorithm dsignAlgo => ToCBOR (Delegation dsignAlgo) where
  toCBOR delegation =
    encodeListLen 2
      <> toCBOR (_delegator delegation)
      <> toCBOR (_delegatee delegation)

makeLenses ''Delegation
