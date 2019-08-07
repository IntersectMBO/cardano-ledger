{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
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
  , mkConfig
  , mkConfigFromFile
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..))
import Data.Coerce (coerce)
import Data.Time (UTCTime)
import Formatting (build, bprint, string)
import qualified Formatting.Buildable as B
import Text.Megaparsec.Error (ParseErrorBundle)

import Cardano.Binary (Annotated(..), Raw)
import Cardano.Chain.Block.Header (HeaderHash, genesisHeaderHash)
import Cardano.Chain.Common (BlockCount, LovelaceError, LovelacePortionError)
import Cardano.Chain.Genesis.Data
  (GenesisData(..), GenesisDataError, readGenesisData)
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import Cardano.Chain.Genesis.Generate
  (GeneratedSecrets, GenesisDataGenerationError, generateGenesisData)
import Cardano.Chain.Genesis.Spec (GenesisSpec(..))
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
  , hash
  )


--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data Config = Config
    { configGenesisData       :: GenesisData
    -- ^ The data needed at genesis
    , configGenesisHash       :: GenesisHash
    -- ^ The hash of the canonical JSON representation of the 'GenesisData'
    , configReqNetMagic       :: RequiresNetworkMagic
    -- ^ Differentiates between Testnet and Mainet/Staging
    , configUTxOConfiguration :: UTxOConfiguration
    -- ^ Extra local data used in UTxO validation rules
    }

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

mkConfigFromFile
  :: (MonadError ConfigurationError m, MonadIO m)
  => RequiresNetworkMagic
  -> FilePath
  -> Hash Raw
  -- ^ This hash comes from 'CardanoConfiguration'
  -- which lives in cardano-shell
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

mkConfig
  :: MonadError ConfigurationError m => UTCTime -> GenesisSpec -> m (Config, GeneratedSecrets)
mkConfig startTime genesisSpec = do
  (genesisData, generatedSecrets) <-
    generateGenesisData startTime genesisSpec
      `wrapError` ConfigurationGenerationError


  let config = Config
        { configGenesisData      = genesisData
        , configGenesisHash      = genesisHash
        , configReqNetMagic = getRequiresNetworkMagic (gsProtocolMagic genesisSpec)
        , configUTxOConfiguration = defaultUTxOConfiguration
        }
  return (config, generatedSecrets)
  where
    -- Anything will do for the genesis hash. A hash of "patak" was used before,
    -- and so it remains.
        genesisHash = GenesisHash $ coerce $ hash @Text "patak"

data ConfigurationError
  = MissingSystemStartTime
  -- ^ A system start time must be given when a testnet genesis is used
  | UnnecessarySystemStartTime
  -- ^ Must not give a custom system start time when using a mainnet genesis
  | ConfigurationGenesisDataError GenesisDataError
  -- ^ An error in constructing 'GenesisData'
  | GenesisHashMismatch GenesisHash (Hash Raw)
  -- ^ The GenesisData canonical JSON hash is different than expected
  | MeaninglessSeed
  -- ^ Custom seed was provided, but it doesn't make sense
  | ConfigurationGenerationError GenesisDataGenerationError
  | GenesisHashDecodeError Text
  -- ^ An error occured while decoding the genesis hash.
  | ConfigParsingError (ParseErrorBundle Text Void)
  -- ^ An error occured while parsing 'CardanoConfiguration'.
  | ConfigPortionConvErr LovelacePortionError
  -- ^ An error occured while converting from a value
  -- from 'CardanoConfiguration' to 'LovelacePortion'.
  | ConfigLovelaceConvErr LovelaceError
  -- ^ An error occured while converting a value
  -- from 'CardanoConfiguration' to 'Lovelace'.
  deriving (Show)

instance B.Buildable ConfigurationError where
  build = \case
    MissingSystemStartTime ->
      bprint "Missing system start time."
    UnnecessarySystemStartTime ->
      bprint "Cannot give a custom start time when using a mainnet genesis."
    ConfigurationGenesisDataError genesisDataError ->
      bprint ("Error in constructing GenesisData: "
             . build
             )
             genesisDataError
    GenesisHashMismatch genesisHash expectedHash ->
      bprint ("GenesisData canonical JSON hash is different than expected. GenesisHash: "
             . string
             . " Expected hash: "
             . string
             )
             (show genesisHash)
             (show expectedHash)
    MeaninglessSeed ->
      bprint "Custom seed was provided but it does not make sense"
    ConfigurationGenerationError genesisDataGenerationError ->
      bprint ("Configuration GenenerationError"
             . build
             )
             genesisDataGenerationError
    GenesisHashDecodeError decodeErr ->
     bprint ("GenesisHashDecodeError: "
            . string
            )
            (toS decodeErr)
    ConfigParsingError pErr ->
      bprint string (show pErr)
    ConfigPortionConvErr err ->
      bprint ("ConfigPortionConvErr: "
             . build
             )
             err
    ConfigLovelaceConvErr err ->
      bprint ("ConfigLovelaceConvErr: "
             . build
             )
             err
