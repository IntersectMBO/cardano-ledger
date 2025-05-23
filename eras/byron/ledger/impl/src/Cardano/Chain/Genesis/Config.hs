{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.Genesis.Config (
  Config (..),
  ConfigurationError (..),
  configGenesisHeaderHash,
  configK,
  configSlotSecurityParam,
  configChainQualityThreshold,
  configEpochSlots,
  configProtocolMagic,
  configProtocolMagicId,
  configGenesisKeyHashes,
  configHeavyDelegation,
  configStartTime,
  configNonAvvmBalances,
  configProtocolParameters,
  configAvvmDistr,
  mkConfigFromFile,
) where

import Cardano.Chain.Block.Header (HeaderHash, genesisHeaderHash)
import Cardano.Chain.Common (BlockCount)
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances (..))
import Cardano.Chain.Genesis.Data (
  GenesisData (..),
  GenesisDataError,
  readGenesisData,
 )
import Cardano.Chain.Genesis.Delegation (GenesisDelegation)
import Cardano.Chain.Genesis.Hash (GenesisHash (..))
import Cardano.Chain.Genesis.KeyHashes (GenesisKeyHashes)
import Cardano.Chain.Genesis.NonAvvmBalances (GenesisNonAvvmBalances)
import Cardano.Chain.ProtocolConstants (
  kChainQualityThreshold,
  kEpochSlots,
  kSlotSecurityParam,
 )
import Cardano.Chain.Slotting (EpochSlots, SlotCount)
import Cardano.Chain.UTxO.UTxOConfiguration (
  UTxOConfiguration,
  defaultUTxOConfiguration,
 )
import Cardano.Chain.Update (ProtocolParameters)
import Cardano.Crypto (
  AProtocolMagic (..),
  Hash,
  ProtocolMagic,
  ProtocolMagicId (..),
  RequiresNetworkMagic,
 )
import Cardano.Crypto.Raw (Raw)
import Cardano.Ledger.Binary (
  Annotated (..),
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import Data.Time (UTCTime)
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data Config = Config
  { configGenesisData :: !GenesisData
  -- ^ The data needed at genesis
  , configGenesisHash :: !GenesisHash
  -- ^ The hash of the canonical JSON representation of the 'GenesisData'
  , configReqNetMagic :: !RequiresNetworkMagic
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
mkConfigFromFile ::
  (MonadError ConfigurationError m, MonadIO m) =>
  RequiresNetworkMagic ->
  FilePath ->
  -- | The expected hash of the file
  Hash Raw ->
  m Config
mkConfigFromFile rnm fp expectedHash = do
  (genesisData, genesisHash) <-
    (`wrapError` ConfigurationGenesisDataError)
      =<< runExceptT
        (readGenesisData fp)

  (unGenesisHash genesisHash == expectedHash)
    `orThrowError` GenesisHashMismatch genesisHash expectedHash

  pure
    $ Config
      { configGenesisData = genesisData
      , configGenesisHash = genesisHash
      , configReqNetMagic = rnm
      , configUTxOConfiguration = defaultUTxOConfiguration -- TODO: add further config plumbing
      }

data ConfigurationError
  = -- | An error in constructing 'GenesisData'
    ConfigurationGenesisDataError GenesisDataError
  | -- | The GenesisData canonical JSON hash is different than expected
    GenesisHashMismatch GenesisHash (Hash Raw)
  | -- | An error occured while decoding the genesis hash.
    GenesisHashDecodeError Text
  deriving (Show)

instance ToCBOR Config where
  toCBOR = toByronCBOR

instance FromCBOR Config where
  fromCBOR = fromByronCBOR

instance EncCBOR Config where
  encCBOR
    ( Config
        configGenesisData_
        configGenesisHash_
        configReqNetMagic_
        configUTxOConfiguration_
      ) =
      mconcat
        [ encodeListLen 4
        , encCBOR @GenesisData configGenesisData_
        , encCBOR @GenesisHash configGenesisHash_
        , encCBOR @RequiresNetworkMagic configReqNetMagic_
        , encCBOR @UTxOConfiguration configUTxOConfiguration_
        ]

instance DecCBOR Config where
  decCBOR = do
    enforceSize "Config" 4
    Config
      <$> decCBOR @GenesisData
      <*> decCBOR @GenesisHash
      <*> decCBOR @RequiresNetworkMagic
      <*> decCBOR @UTxOConfiguration
