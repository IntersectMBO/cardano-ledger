{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shelley.Spec.Ledger.Genesis
  ( ShelleyGenesisStaking (..),
    ShelleyGenesis (..),
    emptyGenesisStaking,
    sgActiveSlotCoeff,
    genesisUtxO,
    initialFundsPseudoTxIn,
  )
where

import Cardano.Crypto (ProtocolMagicId)
import qualified Cardano.Crypto.Hash.Class as Crypto (Hash (..), hash)
import Cardano.Prelude (NoUnexpectedThunks)
import Cardano.Slotting.Slot (EpochSize)
import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Crypto (Crypto, HASH)
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.TxData
import Shelley.Spec.Ledger.UTxO

-- | Genesis Shelley staking configuration.
--
-- This allows us to configure some initial stake pools and delegation to them,
-- in order to test Praos in a static configuration, without requiring on-chain
-- registration and delegation.
--
-- For simplicity, pools defined in the genesis staking do not pay deposits for
-- their registration.
data ShelleyGenesisStaking c = ShelleyGenesisStaking
  { -- | Pools to register
    --
    --   The key in this map is the hash of the public key of the _pool_. This
    --   need not correspond to any payment or staking key, but must correspond
    --   to the cold key held by 'TPraosIsCoreNode'.
    sgsPools :: !(Map (KeyHash 'StakePool c) (PoolParams c)),
    -- | Stake-holding key hash credentials and the pools to delegate that stake
    -- to. We require the raw staking key hash in order to:
    --
    -- - Avoid pointer addresses, which would be tricky when there's no slot or
    --   transaction to point to.
    -- - Avoid script credentials.
    sgsStake :: !(Map (KeyHash 'Staking c) (KeyHash 'StakePool c))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Empty genesis staking
emptyGenesisStaking :: ShelleyGenesisStaking c
emptyGenesisStaking =
  ShelleyGenesisStaking
    { sgsPools = Map.empty,
      sgsStake = Map.empty
    }

-- | Shelley genesis information
--
-- Note that this is needed only for a pure Shelley network, hence it being
-- defined here rather than in its own module. In mainnet, Shelley will
-- transition naturally from Byron, and thus will never have its own genesis
-- information.
data ShelleyGenesis c = ShelleyGenesis
  { sgSystemStart :: !UTCTime,
    sgNetworkMagic :: !Word32,
    sgNetworkId :: !Network,
    sgProtocolMagicId :: !ProtocolMagicId,
    sgActiveSlotsCoeff :: !Double,
    sgSecurityParam :: !Word64,
    sgEpochLength :: !EpochSize,
    sgSlotsPerKESPeriod :: !Word64,
    sgMaxKESEvolutions :: !Word64,
    sgSlotLength :: !NominalDiffTime,
    sgUpdateQuorum :: !Word64,
    sgMaxLovelaceSupply :: !Word64,
    sgProtocolParams :: !PParams,
    sgGenDelegs :: !(Map (KeyHash 'Genesis c) (GenDelegPair c)),
    sgInitialFunds :: !(Map (Addr c) Coin),
    sgStaking :: !(ShelleyGenesisStaking c)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

sgActiveSlotCoeff :: ShelleyGenesis c -> ActiveSlotCoeff
sgActiveSlotCoeff =
  mkActiveSlotCoeff
    . unitIntervalFromRational
    . toRational
    . sgActiveSlotsCoeff

instance Crypto crypto => ToJSON (ShelleyGenesis crypto) where
  toJSON sg =
    Aeson.object
      [ "systemStart" .= sgSystemStart sg,
        --TODO: this should not have both network magic and protocol magic
        -- they are different names for the same thing used in two ways.
        "networkMagic" .= sgNetworkMagic sg,
        "networkId" .= sgNetworkId sg,
        "protocolMagicId" .= sgProtocolMagicId sg,
        "activeSlotsCoeff" .= sgActiveSlotsCoeff sg,
        "securityParam" .= sgSecurityParam sg,
        "epochLength" .= sgEpochLength sg,
        "slotsPerKESPeriod" .= sgSlotsPerKESPeriod sg,
        "maxKESEvolutions" .= sgMaxKESEvolutions sg,
        "slotLength" .= sgSlotLength sg,
        "updateQuorum" .= sgUpdateQuorum sg,
        "maxLovelaceSupply" .= sgMaxLovelaceSupply sg,
        "protocolParams" .= sgProtocolParams sg,
        "genDelegs" .= sgGenDelegs sg,
        "initialFunds" .= sgInitialFunds sg,
        "staking" .= Null
      ]

instance Crypto crypto => FromJSON (ShelleyGenesis crypto) where
  parseJSON =
    Aeson.withObject "ShelleyGenesis" $ \obj ->
      ShelleyGenesis
        <$> obj .: "systemStart"
        <*> obj .: "networkMagic"
        <*> obj .: "networkId"
        <*> obj .: "protocolMagicId"
        <*> obj .: "activeSlotsCoeff"
        <*> obj .: "securityParam"
        <*> obj .: "epochLength"
        <*> obj .: "slotsPerKESPeriod"
        <*> obj .: "maxKESEvolutions"
        <*> obj .: "slotLength"
        <*> obj .: "updateQuorum"
        <*> obj .: "maxLovelaceSupply"
        <*> obj .: "protocolParams"
        <*> obj .: "genDelegs"
        <*> obj .: "initialFunds"
        <*> pure emptyGenesisStaking --TODO

{-------------------------------------------------------------------------------
  Genesis UTxO
-------------------------------------------------------------------------------}

genesisUtxO :: Crypto c => ShelleyGenesis c -> UTxO c
genesisUtxO genesis =
  UTxO $
    Map.fromList
      [ (txIn, txOut)
        | (addr, amount) <- Map.toList (sgInitialFunds genesis),
          let txIn = initialFundsPseudoTxIn addr
              txOut = TxOut addr amount
      ]

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
initialFundsPseudoTxIn :: forall c. Crypto c => Addr c -> TxIn c
initialFundsPseudoTxIn addr =
  TxIn pseudoTxId 0
  where
    pseudoTxId = TxId . castHash $ Crypto.hash addr
    --TODO: move this to the hash API module
    castHash :: Crypto.Hash (HASH c) a -> Crypto.Hash (HASH c) b
    castHash (Crypto.UnsafeHash h) = Crypto.UnsafeHash h
