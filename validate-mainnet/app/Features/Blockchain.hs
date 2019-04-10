{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Features.Blockchain
  ( BlockchainLayer(..)
  , genesisConfig
  , createBlockchainFeature
  , configEpochFileDir
  )
where

import Cardano.Prelude

import Formatting (build, sformat)

import Cardano.Chain.Block
  (ChainValidationState, HeapSize(..), UTxOSize(..), initialChainValidationState, scanUTxOfromGenesis)
import Cardano.Chain.Common (parseReqNetworkMag)
import Cardano.Chain.Epoch.Validation (EpochError, validateEpochFiles)
import Cardano.Chain.Genesis (configProtocolMagic)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Slotting (EpochSlots(..),FlatSlotId(..), SlotId, unflattenSlotId)
import Cardano.Chain.Txp (UTxO(..), genesisUtxo)
import Cardano.Mirror (mainnetEpochFiles)
import Cardano.Shell.Constants.Types (CardanoConfiguration(..), Core(..), Genesis(..))
import Cardano.Shell.Types
  (ApplicationEnvironment(..), CardanoEnvironment, CardanoFeature(..))

-- | `BlockchainLayer` provides a window of sorts that
-- enables us to access values from various 'MVar's
-- that hold the results of specific applications.
data BlockchainLayer = BlockchainLayer
  { chainValidationStatus
      :: forall m
       . MonadIO m
      => m (Maybe (Either EpochError ChainValidationState))
  , currentUTxOSize
      :: forall m
      . MonadIO m
      => m (Maybe (HeapSize UTxO, UTxOSize, SlotId))
  }

data BlockchainConfiguration = BlockchainConfiguration
  { configEpochFileDir :: FilePath
  , genesisConfig      :: Genesis.Config
  }

cleanup :: forall m . MonadIO m => m ()
cleanup = pure ()

-- TODO: I believe this should get the epoch files locally
-- however for now I use `cardano-mainnet-mirror`.
-- TODO: Implement a more granular approach to configuring the `init` function.
-- i.e sometimes we may not want to execute `validateEpochFiles` but only
-- execute `scanUTxOfromGenesis`.
init
  :: forall m
   . MonadIO m
  => BlockchainConfiguration
  -> ApplicationEnvironment
  -> ChainValidationState
  -> MVar (Either EpochError ChainValidationState)
  -> MVar (HeapSize UTxO, UTxOSize, SlotId)
  -> m ()
init config appEnv initialCVS cvsVar utxoVar = do
    files <- case appEnv of
      Development -> take 10 <$> liftIO mainnetEpochFiles
      Production  -> liftIO mainnetEpochFiles

    -- Validate epoch files.

    -- Currently `validateEpochFiles` is restricted to the first 5 epochs.
    result <- liftIO $ validateEpochFiles (genesisConfig config) initialCVS (take 5 files)
    liftIO $ putMVar cvsVar result

    -- Output UTxO size
    let pm = configProtocolMagic $ genesisConfig config

    -- Because we are updating an 'MVar' with the new UTxO size after each
    -- block application (i.e takeMVar then putMVar), we must start with a dummy value.
    liftIO $ putMVar utxoVar dummyUTxoVal
    _ <- liftIO $ scanUTxOfromGenesis pm (genesisUtxo $ genesisConfig config) utxoVar files
    return ()
  where
    -- A dummy value that is inserted into the utxoVar so that folding can
    -- proceed in 'scanUTxOfromGenesis'.
    dummyUTxoVal :: (HeapSize UTxO, UTxOSize, SlotId)
    dummyUTxoVal = (HeapSize 0, UTxOSize 0, unflattenSlotId (EpochSlots 1) (FlatSlotId 1))

createBlockchainFeature
  :: CardanoEnvironment
  -> CardanoConfiguration
  -> ApplicationEnvironment
  -> IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature _ cc appEnv = do

  -- Construct `Config` using mainnet-genesis.json
  let mainnetGenFilepath = geSrc . coGenesis $ ccCore cc
  let reqNetworkMagic = parseReqNetworkMag . coRequiresNetworkMagic $ ccCore cc

  config <- either (panic . sformat build) identity
    <$> runExceptT
          (Genesis.mkConfigFromFile reqNetworkMagic mainnetGenFilepath Nothing)

  let
    blockchainConf =
      BlockchainConfiguration "cardano-mainnet-mirror/epochs" config

  -- Create initial `ChainValidationState`.
  initCVS <- either (panic . show) identity
    <$> runExceptT (initialChainValidationState config)

  -- Create MVar that will hold the result of the bulk chain validation.
  cvsVar <- newEmptyMVar

  -- Create MVar that will hold the current size of the 'UTxO' and 'SlotId'
  utxoSizeVar <- newEmptyMVar

  -- Create Blockchain feature
  let
    bcFeature = CardanoFeature
      { featureName     = "Blockchain"
      -- `featureStart` is the logic to be executed of a given feature.
      , featureStart    = init blockchainConf appEnv initCVS cvsVar utxoSizeVar
      , featureShutdown = cleanup
      }

  -- Create `BlockchainLayer` which allows us to see the status of
  -- the blockchain feature.
  let
    bcLayer =
      BlockchainLayer
        { chainValidationStatus = liftIO $ tryReadMVar cvsVar
        , currentUTxOSize       = liftIO $ tryReadMVar utxoSizeVar
        }
  pure (bcLayer, bcFeature)


