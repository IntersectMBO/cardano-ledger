{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Genesis.Config
  ( StaticConfig(..)
  , Config(..)
  , ConfigurationError(..)
  , configGenesisHeaderHash
  , configK
  , configSlotSecurityParam
  , configChainQualityThreshold
  , configEpochSlots
  , configProtocolMagic
  , configProtocolMagicId
  , configGeneratedSecretsThrow
  , configGenesisKeyHashes
  , configHeavyDelegation
  , configStartTime
  , configNonAvvmBalances
  , configProtocolParameters
  , configAvvmDistr
  , mkConfig
  , mkConfigFromFile
  , mkConfigFromStaticConfig
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..))
import Data.Coerce (coerce)
import Data.Time (UTCTime)
import Formatting (build, bprint, string)
import qualified Formatting.Buildable as B
import System.IO.Error (userError)
import Text.Megaparsec.Error (ParseErrorBundle)

import Cardano.Binary (Annotated(..), Raw)
import Cardano.Chain.Block.Header (HeaderHash, genesisHeaderHash)
import Cardano.Chain.Common (BlockCount, LovelaceError, LovelacePortionError)
import Cardano.Chain.Genesis.Data
  (GenesisData(..), GenesisDataError, readGenesisData)
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import Cardano.Chain.Genesis.Initializer (GenesisInitializer(..))
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
-- StaticConfig
--------------------------------------------------------------------------------

data StaticConfig
  = GCSpec !GenesisSpec
  -- ^ Genesis from a 'GenesisSpec'
  | GCSrc !FilePath !(Hash Raw)
  -- ^ 'GenesisData' is stored in at 'FilePath' with expected 'Hash Raw'
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data Config = Config
    { configGenesisData       :: GenesisData
    -- ^ The data needed at genesis
    , configGenesisHash       :: GenesisHash
    -- ^ The hash of the canonical JSON representation of the 'GenesisData'
    , configGeneratedSecrets  :: Maybe GeneratedSecrets
    -- ^ Secrets needed to access 'GenesisData' in testing
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


configGeneratedSecretsThrow :: MonadIO m => Config -> m GeneratedSecrets
configGeneratedSecretsThrow =
  maybe
      (liftIO $ throwIO $ userError
        "GeneratedSecrets missing from Genesis.Config"
      )
      pure
    . configGeneratedSecrets

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

-- | Construct a 'Config' from a 'StaticConfig'
--
--   If the 'StaticConfig' refers to a canonical JSON file, then it will be
--   hashed and checked against the expected hash.
--
--   If the 'StaticConfig' contains a 'GenesisSpec', then a full 'GenesisData'
--   will be generated. In this case a start time must be provided.
mkConfigFromStaticConfig
  :: (MonadError ConfigurationError m, MonadIO m)
  => RequiresNetworkMagic
  -> Maybe UTCTime
  -- ^ Optional system start time.
  --   It must be given when the genesis spec uses a testnet initializer.
  -> Maybe Integer
  -- ^ Optional seed which overrides one from testnet initializer if provided
  -> StaticConfig
  -> m Config
mkConfigFromStaticConfig rnm mSystemStart mSeed = \case
  -- If a 'GenesisData' source file is given, we check its hash against the
  -- given expected hash, parse it, and use the GenesisData to fill in all of
  -- the obligations.
  GCSrc fp expectedHash -> do

    isNothing mSystemStart `orThrowError` UnnecessarySystemStartTime

    isNothing mSeed `orThrowError` MeaninglessSeed

    mkConfigFromFile rnm fp expectedHash


  -- If a 'GenesisSpec' is given, we ensure we have a start time (needed if it's
  -- a testnet initializer) and then make a 'GenesisData' from it.
  GCSpec spec -> do

    systemStart <- maybe (throwError MissingSystemStartTime) pure mSystemStart

    -- Override seed if necessary
    let
      overrideSeed :: Integer -> GenesisInitializer -> GenesisInitializer
      overrideSeed newSeed gi = gi { giSeed = newSeed }

    let
      spec' = case mSeed of
        Nothing -> spec
        Just newSeed ->
          spec { gsInitializer = overrideSeed newSeed (gsInitializer spec) }

    mkConfig systemStart spec'

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
    , configGeneratedSecrets  = Nothing
    , configReqNetMagic       = rnm
    , configUTxOConfiguration = defaultUTxOConfiguration --TODO: add further config plumbing
    }

mkConfig
  :: MonadError ConfigurationError m => UTCTime -> GenesisSpec -> m Config
mkConfig startTime genesisSpec = do
  (genesisData, generatedSecrets) <-
    generateGenesisData startTime genesisSpec
      `wrapError` ConfigurationGenerationError


  pure $ Config
    { configGenesisData      = genesisData
    , configGenesisHash      = genesisHash
    , configGeneratedSecrets = Just generatedSecrets
    , configReqNetMagic = getRequiresNetworkMagic (gsProtocolMagic genesisSpec)
    , configUTxOConfiguration = defaultUTxOConfiguration
    }
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
