{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Config (
  readMainetCfg,
) where

import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Crypto.Hashing (Hash, decodeHash)
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import Cardano.Crypto.Raw (Raw)
import Cardano.Prelude
import Paths_cardano_ledger_byron (getDataFileName)

-- | Read the test mainnet configuration file from the @test@ directory.
--
-- An error is thrown if it is not possible to elaborate a genesis
-- configuration from the genesis file.
--
-- We use `RequiresNoMagic`, as it indicates mainnet
readMainetCfg :: MonadIO m => m Genesis.Config
readMainetCfg = do
  mainnetGenesisJson <- liftIO $ getDataFileName "test/mainnet-genesis.json"
  let genHash =
        either
          (panic . show . Genesis.GenesisHashDecodeError)
          identity
          ( decodeHash
              "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
          ) ::
          Hash Raw

  either (panic . show) identity
    <$> runExceptT
      (Genesis.mkConfigFromFile RequiresNoMagic mainnetGenesisJson genHash)
