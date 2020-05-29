{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Shelley.Spec.Ledger.Genesis
  ( ShelleyGenesisStaking(..)
  , ShelleyGenesis(..)
  , emptyGenesisStaking
  , sgActiveSlotCoeff
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

import Cardano.Crypto (ProtocolMagicId)
import Cardano.Prelude (Natural, NoUnexpectedThunks)
import Cardano.Slotting.Slot (EpochSize)

import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.TxData

-- | Genesis Shelley staking configuration.
--
-- This allows us to configure some initial stake pools and delegation to them,
-- in order to test Praos in a static configuration, without requiring on-chain
-- registration and delegation.
--
-- For simplicity, pools defined in the genesis staking do not pay deposits for
-- their registration.
data ShelleyGenesisStaking c = ShelleyGenesisStaking {
      -- | Pools to register
      --
      --   The key in this map is the hash of the public key of the _pool_. This
      --   need not correspond to any payment or staking key, but must correspond
      --   to the cold key held by 'TPraosIsCoreNode'.
      sgsPools :: !(Map (KeyHash 'StakePool c) (PoolParams c))
      -- | Stake-holding key hash credentials and the pools to delegate that stake
      -- to. We require the raw staking key hash in order to:
      --
      -- - Avoid pointer addresses, which would be tricky when there's no slot or
      --   transaction to point to.
      -- - Avoid script credentials.
    , sgsStake :: !(Map (KeyHash 'Staking c) (KeyHash 'StakePool c))
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Empty genesis staking
emptyGenesisStaking :: ShelleyGenesisStaking c
emptyGenesisStaking = ShelleyGenesisStaking
  { sgsPools = Map.empty
  , sgsStake = Map.empty
  }

-- | Shelley genesis information
--
-- Note that this is needed only for a pure Shelley network, hence it being
-- defined here rather than in its own module. In mainnet, Shelley will
-- transition naturally from Byron, and thus will never have its own genesis
-- information.
data ShelleyGenesis c = ShelleyGenesis {
      sgSystemStart           :: !UTCTime
    , sgNetworkMagic          :: !Word32
    , sgNetworkId             :: !Network
    , sgProtocolMagicId       :: !ProtocolMagicId
    , sgActiveSlotsCoeff      :: !Double
    , sgSecurityParam         :: !Word64
    , sgEpochLength           :: !EpochSize
    , sgSlotsPerKESPeriod     :: !Word64
    , sgMaxKESEvolutions      :: !Word64
    , sgSlotLength            :: !NominalDiffTime
    , sgUpdateQuorum          :: !Word64
    , sgMaxMajorPV            :: !Natural
    , sgMaxLovelaceSupply     :: !Word64
    , sgProtocolParams        :: !PParams
    , sgGenDelegs
        :: !(Map
              (KeyHash 'Genesis c)
              (KeyHash 'GenesisDelegate c, Hash c (VerKeyVRF c))
            )
    , sgInitialFunds          :: !(Map (Addr c) Coin)
    , sgStaking               :: !(ShelleyGenesisStaking c)
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

sgActiveSlotCoeff :: ShelleyGenesis c -> ActiveSlotCoeff
sgActiveSlotCoeff =
      mkActiveSlotCoeff
    . truncateUnitInterval
    . toRational
    . sgActiveSlotsCoeff
