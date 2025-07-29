{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides the 'StakePoolState' data type, which represents the
-- state of a stake pool within the ledger. Unlike 'PoolParams', which includes
-- the pool ID and is used for pool registration and updates, 'StakePoolState'
-- is designed specifically for state management and excludes the pool ID
-- (since it's already used as the key in state maps).
--
-- This separation allows for:
-- * More efficient state storage (no redundant pool ID)
-- * Future extensibility of pool state without affecting registration parameters
-- * Clear distinction between registration parameters and actual pool state
--
-- The module also provides the deprecation path for 'PoolParams' functionality,
-- which will eventually be moved here.
module Cardano.Ledger.State.StakePool (
  -- * Stake Pool State
  StakePoolState (..),

  -- * Conversions
  mkStakePoolState,
  stakePoolStateToPoolParams,

  -- * Re-exports from PoolParams

  -- | These types are re-exported here as part of the migration from
  -- 'Cardano.Ledger.PoolParams'. New code should import them from here.
  PoolParams (..),
  PoolMetadata (..),
  StakePoolRelay (..),
  SizeOfPoolRelays (..),
  SizeOfPoolOwners (..),
) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (
  StrictMaybe,
  UnitInterval,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.PoolParams (
  PoolMetadata (..),
  PoolParams (..),
  SizeOfPoolOwners (..),
  SizeOfPoolRelays (..),
  StakePoolRelay (..),
 )
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | State representation of a stake pool. This type contains all the same
-- information as 'PoolParams' except for the pool ID, which is stored
-- separately as the key in state maps.
--
-- This type is era-parametric to allow for future extensions without
-- breaking existing code.
data StakePoolState era = StakePoolState
  { spsVrf :: !(VRFVerKeyHash 'StakePoolVRF)
  -- ^ VRF verification key hash for leader election
  , spsPledge :: !Coin
  -- ^ Pledge amount committed by the pool operator
  , spsCost :: !Coin
  -- ^ Fixed operational cost per epoch
  , spsMargin :: !UnitInterval
  -- ^ Pool profit margin (variable fee percentage)
  , spsRewardAccount :: !RewardAccount
  -- ^ Reward account for pool rewards
  , spsOwners :: !(Set (KeyHash 'Staking))
  -- ^ Set of stake key hashes that own this pool
  , spsRelays :: !(StrictSeq StakePoolRelay)
  -- ^ Network relay information for pool connectivity
  , spsMetadata :: !(StrictMaybe PoolMetadata)
  -- ^ Optional metadata (URL and hash)
  }
  deriving (Show, Generic, Eq, Ord)

deriving instance NoThunks (StakePoolState era)

deriving instance NFData (StakePoolState era)

deriving instance ToJSON (StakePoolState era)

deriving instance FromJSON (StakePoolState era)

instance Era era => EncCBOR (StakePoolState era) where
  encCBOR sps =
    encode $
      Rec StakePoolState
        !> To (spsVrf sps)
        !> To (spsPledge sps)
        !> To (spsCost sps)
        !> To (spsMargin sps)
        !> To (spsRewardAccount sps)
        !> To (spsOwners sps)
        !> To (spsRelays sps)
        !> To (spsMetadata sps)

instance Era era => DecCBOR (StakePoolState era) where
  decCBOR =
    decode $
      RecD StakePoolState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance Era era => DecShareCBOR (StakePoolState era) where
  decShareCBOR _ = decCBOR

instance Default (StakePoolState era) where
  def =
    StakePoolState
      { spsVrf = def
      , spsPledge = Coin 0
      , spsCost = Coin 0
      , spsMargin = def
      , spsRewardAccount = def
      , spsOwners = def
      , spsRelays = def
      , spsMetadata = def
      }

-- | Convert 'PoolParams' to 'StakePoolState' by dropping the pool ID.
-- This is the primary way to create a 'StakePoolState' from registration
-- or update parameters.
mkStakePoolState :: PoolParams -> StakePoolState era
mkStakePoolState pp =
  StakePoolState
    { spsVrf = ppVrf pp
    , spsPledge = ppPledge pp
    , spsCost = ppCost pp
    , spsMargin = ppMargin pp
    , spsRewardAccount = ppRewardAccount pp
    , spsOwners = ppOwners pp
    , spsRelays = ppRelays pp
    , spsMetadata = ppMetadata pp
    }

-- | Convert 'StakePoolState' back to 'PoolParams' by providing the pool ID.
-- This is useful when you need to reconstruct the full parameters from
-- the state representation.
stakePoolStateToPoolParams :: KeyHash 'StakePool -> StakePoolState era -> PoolParams
stakePoolStateToPoolParams poolId sps =
  PoolParams
    { ppId = poolId
    , ppVrf = spsVrf sps
    , ppPledge = spsPledge sps
    , ppCost = spsCost sps
    , ppMargin = spsMargin sps
    , ppRewardAccount = spsRewardAccount sps
    , ppOwners = spsOwners sps
    , ppRelays = spsRelays sps
    , ppMetadata = spsMetadata sps
    }
