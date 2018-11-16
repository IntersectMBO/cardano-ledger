{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Genesis.Data
  ( GenesisData(..)
  , readGenesisData
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time (UTCTime)
import Formatting (bprint, build, stext)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical
  (FromJSON(..), ToJSON(..), fromJSField, mkObject, parseCanonicalJSON)

import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances)
import Cardano.Chain.Genesis.Delegation (GenesisDelegation)
import Cardano.Chain.Genesis.NonAvvmBalances (GenesisNonAvvmBalances)
import Cardano.Chain.Genesis.ProtocolConstants (GenesisProtocolConstants)
import Cardano.Chain.Genesis.WStakeholders (GenesisWStakeholders)
import Cardano.Chain.Update.BlockVersionData (BlockVersionData)


-- | Genesis data contains all data which determines consensus rules. It must be
--   same for all nodes. It's used to initialize global state, slotting, etc.
data GenesisData = GenesisData
    { gdBootStakeholders :: !GenesisWStakeholders
    , gdHeavyDelegation  :: !GenesisDelegation
    , gdStartTime        :: !UTCTime
    , gdNonAvvmBalances  :: !GenesisNonAvvmBalances
    , gdBlockVersionData :: !BlockVersionData
    , gdProtocolConsts   :: !GenesisProtocolConstants
    , gdAvvmDistr        :: !GenesisAvvmBalances
    } deriving (Show, Eq)

instance Monad m => ToJSON m GenesisData where
  toJSON gd = mkObject
    [ ("bootStakeholders", toJSON $ gdBootStakeholders gd)
    , ("heavyDelegation" , toJSON $ gdHeavyDelegation gd)
    , ("startTime"       , toJSON $ gdStartTime gd)
    , ("nonAvvmBalances" , toJSON $ gdNonAvvmBalances gd)
    , ("blockVersionData", toJSON $ gdBlockVersionData gd)
    , ("protocolConsts"  , toJSON $ gdProtocolConsts gd)
    , ("avvmDistr"       , toJSON $ gdAvvmDistr gd)
    ]

instance MonadError SchemaError m => FromJSON m GenesisData where
  fromJSON obj =
    GenesisData
      <$> fromJSField obj "bootStakeholders"
      <*> fromJSField obj "heavyDelegation"
      <*> fromJSField obj "startTime"
      <*> fromJSField obj "nonAvvmBalances"
      <*> fromJSField obj "blockVersionData"
      <*> fromJSField obj "protocolConsts"
      <*> fromJSField obj "avvmDistr"

data GenesisDataError
  = GenesisDataParseError Text
  | GenesisDataSchemaError SchemaError

instance B.Buildable GenesisDataError where
  build = \case
    GenesisDataParseError err ->
      bprint ("Failed to parse GenesisData.\n Error: " . stext) err
    GenesisDataSchemaError err ->
      bprint ("Incorrect schema for GenesisData.\n Error: " . build) err

readGenesisData
  :: (MonadError GenesisDataError m, MonadIO m) => FilePath -> m GenesisData
readGenesisData fp = do
  bytes           <- liftIO $ BS.readFile fp
  genesisDataJSON <-
    liftEither . first (GenesisDataParseError . toS) $ parseCanonicalJSON
      (BSL.fromStrict bytes)
  liftEither . first GenesisDataSchemaError $ fromJSON genesisDataJSON
