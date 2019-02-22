{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Config
  ( readMainetCfg
  )
where

import Cardano.Prelude

import qualified Cardano.Chain.Genesis as Genesis

-- | Read the test mainnet configuration file from the @test@ directory.
--
-- An error is thrown if it is not possible to elaborate a genesis
-- configuration from the genesis file.
readMainetCfg :: MonadIO m => m Genesis.Config
readMainetCfg =
  either
      (panic "TODO: Add buildable instance for Genesis.ConfigurationError")
      identity
    <$> runExceptT
          (Genesis.mkConfigFromFile "test/mainnet-genesis.json" Nothing)
