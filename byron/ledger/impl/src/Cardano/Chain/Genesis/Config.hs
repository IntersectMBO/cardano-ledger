{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Genesis.Config
  ( Config(..)
  , ConfigurationError(..)
  , configGenesisHeaderHash
  , configK
  , configSlotSecurityParam
  , configChainQualityThreshold
  , configEpochSlots
  , configProtocolMagic
  , configProtocolMagicId
  , configGenesisKeyHashes
  , configHeavyDelegation
  , configStartTime
  , configNonAvvmBalances
  , configProtocolParameters
  , configAvvmDistr
  , mkConfigFromFile
  )
where

import Cardano.Prelude

import Data.Time (UTCTime)
import NoThunks.Class (NoThunks (..))

import Cardano.Binary
  ( Annotated(..)
  , Raw
  , FromCBOR(..)
  , ToCBOR(..)
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Block.Header (HeaderHash, genesisHeaderHash)
import Cardano.Chain.Common (BlockCount)
import Cardano.Chain.Genesis.Data
  (GenesisData(..), GenesisDataError, readGenesisData)
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import Cardano.Chain.Genesis.KeyHashes (GenesisKeyHashes)
import Cardano.Chain.Genesis.Delegation (GenesisDelegation)
import Cardano.Chain.Genesis.NonAvvmBalances (GenesisNonAvvmBalances)
import Cardano.Chain.ProtocolConstants
  (kEpochSlots, kSlotSecurityParam, kChainQualityThreshold)
import Cardano.Chain.Slotting (EpochSlots, SlotCount)
import Cardano.Chain.Update (ProtocolParameters)
import Cardano.Chain.UTxO.UTxOConfiguration
  (UTxOConfiguration, defaultUTxOConfiguration)
import Cardano.Crypto
  ( AProtocolMagic(..)
  , Hash
  , ProtocolMagic
  , ProtocolMagicId(..)
  , RequiresNetworkMagic
  )


--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data Config = Config
    { configGenesisData       :: !GenesisData
    -- ^ The data needed at genesis
    , configGenesisHash       :: !GenesisHash
    -- ^ The hash of the canonical JSON representation of the 'GenesisData'
    , configReqNetMagic       :: !RequiresNetworkMagic
    -- ^ Differentiates between Testnet and Mainet/Staging
    , configUTxOConfiguration :: !UTxOConfiguration
    -- ^ Extra local data used in UTxO validation rules
    }
  deriving (Generic, Eq, Show, NoThunks)

configGenesisHeaderHash :: Config -> HeaderHash
configGenesisHeaderHash = genesisHeaderHash . configGenesisHash

configK :: Config -> BlockCount
configK = gdK . configGenesisData

configSlotSecurityParam :: Config -> SlotCount
configSlotSecurityParam = kSlotSecurityParam . configK

configChainQualityThreshold :: Fractional f => Config -> f
configChainQualityThreshold = kChainQualityThreshold . configK

configEpochSlots :: Config -> EpochSlots
configEpochSlots = kEpochSlots . configK

-- | There isn't a full @ProtocolMagic@ in @Config@, but the requisite
-- @ProtocolMagicId@ and @RequiresNetworkMagic@ are stored separately.
-- We use them to construct and return a @ProtocolMagic@.
configProtocolMagic :: Config -> ProtocolMagic
configProtocolMagic config = AProtocolMagic (Annotated pmi ()) rnm
 where
  pmi = configProtocolMagicId config
  rnm = configReqNetMagic config

configProtocolMagicId :: Config -> ProtocolMagicId
configProtocolMagicId = gdProtocolMagicId . configGenesisData


configGenesisKeyHashes :: Config -> GenesisKeyHashes
configGenesisKeyHashes = gdGenesisKeyHashes . configGenesisData

configHeavyDelegation :: Config -> GenesisDelegation
configHeavyDelegation = gdHeavyDelegation . configGenesisData

configStartTime :: Config -> UTCTime
configStartTime = gdStartTime . configGenesisData

configNonAvvmBalances :: Config -> GenesisNonAvvmBalances
configNonAvvmBalances = gdNonAvvmBalances . configGenesisData

configProtocolParameters :: Config -> ProtocolParameters
configProtocolParameters = gdProtocolParameters . configGenesisData

configAvvmDistr :: Config -> GenesisAvvmBalances
configAvvmDistr = gdAvvmDistr . configGenesisData

-- | Construct a 'Config' from an external genesis file.
--
-- The 'FilePath' refers to a canonical JSON file. It will be hashed and
-- checked against the expected hash, which should be known from config.
--
mkConfigFromFile
  :: (MonadError ConfigurationError m, MonadIO m)
  => RequiresNetworkMagic
  -> FilePath
  -> Hash Raw
  -- ^ The expected hash of the file
  -> m Config
mkConfigFromFile rnm fp expectedHash = do
  (genesisData, genesisHash) <-
    (`wrapError` ConfigurationGenesisDataError) =<< runExceptT
      (readGenesisData fp)

  (unGenesisHash genesisHash == expectedHash)
    `orThrowError` GenesisHashMismatch genesisHash expectedHash

  pure $ Config
    { configGenesisData       = genesisData
    , configGenesisHash       = genesisHash
    , configReqNetMagic       = rnm
    , configUTxOConfiguration = defaultUTxOConfiguration --TODO: add further config plumbing
    }

data ConfigurationError
  = ConfigurationGenesisDataError GenesisDataError
  -- ^ An error in constructing 'GenesisData'
  | GenesisHashMismatch GenesisHash (Hash Raw)
  -- ^ The GenesisData canonical JSON hash is different than expected
  | GenesisHashDecodeError Text
  -- ^ An error occured while decoding the genesis hash.
  deriving (Show)

instance ToCBOR Config where
  toCBOR
    (Config
      configGenesisData_
      configGenesisHash_
      configReqNetMagic_
      configUTxOConfiguration_
    ) = mconcat [
            encodeListLen 4
          , toCBOR @GenesisData configGenesisData_
          , toCBOR @GenesisHash configGenesisHash_
          , toCBOR @RequiresNetworkMagic configReqNetMagic_
          , toCBOR @UTxOConfiguration configUTxOConfiguration_
          ]

instance FromCBOR Config where
  fromCBOR = do
    enforceSize "Config" 4
    Config
      <$> fromCBOR @GenesisData
      <*> fromCBOR @GenesisHash
      <*> fromCBOR @RequiresNetworkMagic
      <*> fromCBOR @UTxOConfiguration
