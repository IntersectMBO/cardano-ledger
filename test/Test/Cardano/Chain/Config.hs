{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Config
  ( readMainetCfg
  , mainnetEpochSlots
  )
where

import Cardano.Prelude

import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic(..))
import Cardano.Chain.Slotting (EpochSlots(EpochSlots))

-- | Read the test mainnet configuration file from the @test@ directory.
--
-- An error is thrown if it is not possible to elaborate a genesis
-- configuration from the genesis file.
--
-- We use `RequiresNoMagic`, as it indicates mainnet
readMainetCfg :: MonadIO m => m Genesis.Config
readMainetCfg =
  either
      (panic "TODO: Add buildable instance for Genesis.ConfigurationError")
      identity
    <$> runExceptT
          (Genesis.mkConfigFromFile RequiresNoMagic "test/mainnet-genesis.json" Nothing)

-- | Slots per epoch used in mainnet
--
-- This number has been fixed throughout the Byron era.
mainnetEpochSlots :: EpochSlots
mainnetEpochSlots = EpochSlots 21600
