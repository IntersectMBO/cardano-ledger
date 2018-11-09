{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Genesis.Config
  ( StaticConfig(..)
  )
where

import Cardano.Prelude

import Data.Aeson
  ( FromJSON
  , ToJSON
  , object
  , pairs
  , parseJSON
  , toEncoding
  , toJSON
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Encoding (pairStr)
import Data.Aeson.Encoding.Internal (pair)

import Cardano.Binary.Class (Raw)
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import Cardano.Chain.Genesis.Initializer (GenesisInitializer(..))
import Cardano.Chain.Genesis.Spec (GenesisSpec(..))
import Cardano.Crypto.Hashing (Hash)

--------------------------------------------------------------------------------
-- StaticConfig
--------------------------------------------------------------------------------

data StaticConfig
    -- | Genesis from a 'GenesisSpec'.
  = GCSpec !GenesisSpec
    -- | 'GenesisData' is stored in a file.
  | GCSrc !FilePath !(Hash Raw)
    -- !FilePath = Path to file where 'GenesisData' is stored. Must be
    -- in JSON, not necessary canonical.
    -- !(Hash Raw) = Hash of canonically encoded 'GenesisData'.
  deriving (Eq, Show)

instance ToJSON StaticConfig where
  toJSON (GCSrc gcsFile gcsHash) =
      object [ "src"    .= object [ "file" .= gcsFile
                                  , "hash" .= gcsHash
                                  ]
             ]
  toJSON (GCSpec value) = object ["spec" .= toJSON value]

  toEncoding (GCSrc gcsFile gcsHash) =
      pairs $ "src" `pair`
          pairs (mconcat [ "file" .= gcsFile
                           , "hash" .= gcsHash
                           ])
  toEncoding (GCSpec value) = pairs $ pairStr "spec" (toEncoding value)

instance FromJSON StaticConfig where
  parseJSON = withObject "StaticConfig" $ \o -> do
    src <- o .:? "src"
    case src of
      Just src' -> do
        file <- src' .: "file"
        hash <- src' .: "hash"
        pure $ GCSrc file hash
      Nothing -> do
        specO <- o .: "spec"
        -- GenesisAvvmBalances
        avvmDistrV <- specO .: "avvmDistr"
        avvmDistr <- parseJSON avvmDistrV
        -- SharedSeed
        ftsSeed <- specO .: "ftsSeed"
        -- GenesisDelegation
        heavyDelegationV <- specO .: "heavyDelegation"
        heavyDelegation <- parseJSON heavyDelegationV
        -- BlockVersionData
        blockVersionDataV <- specO .: "blockVersionData"
        blockVersionData <- parseJSON blockVersionDataV
        -- GenesisProtocolConstants
        protocolConstantsV <- specO .: "protocolConstants"
        protocolConstants <- parseJSON protocolConstantsV
        -- GenesisInitializer
        initializerO <- specO .: "initializer"
        testBalanceV <- initializerO .: "testBalance"
        testBalance <- parseJSON testBalanceV
        fakeAvvmBalanceV <- initializerO .: "fakeAvvmBalance"
        fakeAvvmBalance <- parseJSON fakeAvvmBalanceV
        avvmBalanceFactor <- initializerO .: "avvmBalanceFactor"
        useHeavyDlg <- initializerO .: "useHeavyDlg"
        seed <- initializerO .: "seed"

        return . GCSpec $
          UnsafeGenesisSpec
            (GenesisAvvmBalances avvmDistr)
            ftsSeed
            heavyDelegation
            blockVersionData
            protocolConstants
            (GenesisInitializer
              testBalance
              fakeAvvmBalance
              avvmBalanceFactor
              useHeavyDlg
              seed)
