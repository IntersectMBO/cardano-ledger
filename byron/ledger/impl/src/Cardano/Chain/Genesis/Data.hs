{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Chain.Genesis.Data
  ( GenesisData (..),
    GenesisDataError (..),
    mainnetProtocolMagicId,
    readGenesisData,
  )
where

import Cardano.Binary
import Cardano.Chain.Common (BlockCount (..))
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances)
import Cardano.Chain.Genesis.Delegation (GenesisDelegation)
import Cardano.Chain.Genesis.Hash (GenesisHash (..))
import Cardano.Chain.Genesis.KeyHashes (GenesisKeyHashes)
import Cardano.Chain.Genesis.NonAvvmBalances (GenesisNonAvvmBalances)
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
import Cardano.Crypto
  ( ProtocolMagicId (..),
    hashRaw,
  )
import Cardano.Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (lookup)
import Data.Time (UTCTime)
import Formatting (bprint, build, stext)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical
  ( FromJSON (..),
    Int54,
    JSValue (..),
    ToJSON (..),
    expected,
    fromJSField,
    fromJSObject,
    mkObject,
    parseCanonicalJSON,
    renderCanonicalJSON,
  )

-- | Genesis data contains all data which determines consensus rules. It must be
--   same for all nodes. It's used to initialize global state, slotting, etc.
data GenesisData = GenesisData
  { gdGenesisKeyHashes :: !GenesisKeyHashes,
    gdHeavyDelegation :: !GenesisDelegation,
    gdStartTime :: !UTCTime,
    gdNonAvvmBalances :: !GenesisNonAvvmBalances,
    gdProtocolParameters :: !ProtocolParameters,
    gdK :: !BlockCount,
    gdProtocolMagicId :: !ProtocolMagicId,
    gdAvvmDistr :: !GenesisAvvmBalances
  }
  deriving (Show, Eq, Generic, NoThunks)

instance Monad m => ToJSON m GenesisData where
  toJSON gd =
    mkObject
      [ ("bootStakeholders", toJSON $ gdGenesisKeyHashes gd),
        ("heavyDelegation", toJSON $ gdHeavyDelegation gd),
        ("startTime", toJSON $ gdStartTime gd),
        ("nonAvvmBalances", toJSON $ gdNonAvvmBalances gd),
        ("blockVersionData", toJSON $ gdProtocolParameters gd),
        --  The above is called blockVersionData for backwards compatibility with
        --  mainnet genesis block
        ( "protocolConsts",
          mkObject
            [ ("k", pure . JSNum . fromIntegral . unBlockCount $ gdK gd),
              ("protocolMagic", toJSON $ gdProtocolMagicId gd)
            ]
        ),
        ("avvmDistr", toJSON $ gdAvvmDistr gd)
      ]

instance MonadError SchemaError m => FromJSON m GenesisData where
  fromJSON obj = do
    objAssoc <- fromJSObject obj
    protocolConsts <- case lookup "protocolConsts" objAssoc of
      Just fld -> pure fld
      Nothing -> expected "field protocolConsts" Nothing

    GenesisData
      <$> fromJSField obj "bootStakeholders"
      <*> fromJSField obj "heavyDelegation"
      <*> fromJSField obj "startTime"
      <*> fromJSField obj "nonAvvmBalances"
      <*> fromJSField obj "blockVersionData"
      -- The above is called blockVersionData for backwards compatibility with
      -- mainnet genesis block
      <*> (BlockCount . fromIntegral @Int54 <$> fromJSField protocolConsts "k")
      <*> (ProtocolMagicId <$> fromJSField protocolConsts "protocolMagic")
      <*> fromJSField obj "avvmDistr"

data GenesisDataError
  = GenesisDataParseError Text
  | GenesisDataSchemaError SchemaError
  | GenesisDataIOError IOException
  deriving (Show)

instance B.Buildable GenesisDataError where
  build = \case
    GenesisDataParseError err ->
      bprint ("Failed to parse GenesisData.\n Error: " . stext) err
    GenesisDataSchemaError err ->
      bprint ("Incorrect schema for GenesisData.\n Error: " . build) err
    GenesisDataIOError err ->
      bprint
        ("Failed with " . stext . " when tried to read GenesisData file")
        (show err)

instance ToCBOR GenesisData where
  toCBOR
    ( GenesisData
        gdGenesisKeyHashes_
        gdHeavyDelegation_
        gdStartTime_
        gdNonAvvmBalances_
        gdProtocolParameters_
        gdK_
        gdProtocolMagicId_
        gdAvvmDistr_
      ) =
      mconcat
        [ encodeListLen 8,
          toCBOR @GenesisKeyHashes gdGenesisKeyHashes_,
          toCBOR @GenesisDelegation gdHeavyDelegation_,
          toCBOR {- @UTCTime -} gdStartTime_,
          toCBOR @GenesisNonAvvmBalances gdNonAvvmBalances_,
          toCBOR @ProtocolParameters gdProtocolParameters_,
          toCBOR @BlockCount gdK_,
          toCBOR @ProtocolMagicId gdProtocolMagicId_,
          toCBOR @GenesisAvvmBalances gdAvvmDistr_
        ]

instance FromCBOR GenesisData where
  fromCBOR = do
    enforceSize "GenesisData" 8
    GenesisData
      <$> fromCBOR @GenesisKeyHashes
      <*> fromCBOR @GenesisDelegation
      <*> fromCBOR -- @UTCTime
      <*> fromCBOR @GenesisNonAvvmBalances
      <*> fromCBOR @ProtocolParameters
      <*> fromCBOR @BlockCount
      <*> fromCBOR @ProtocolMagicId
      <*> fromCBOR @GenesisAvvmBalances

-- | Parse @GenesisData@ from a JSON file and annotate with Canonical JSON hash
readGenesisData ::
  (MonadError GenesisDataError m, MonadIO m) =>
  FilePath ->
  m (GenesisData, GenesisHash)
readGenesisData fp = do
  bytes <-
    liftIO (try $ BSL.fromStrict <$> BS.readFile fp)
      >>= (`wrapError` GenesisDataIOError)

  genesisDataJSON <-
    parseCanonicalJSON bytes `wrapError` GenesisDataParseError . toS

  genesisData <- fromJSON genesisDataJSON `wrapError` GenesisDataSchemaError

  let genesisHash = GenesisHash $ hashRaw (renderCanonicalJSON genesisDataJSON)

  pure (genesisData, genesisHash)

mainnetProtocolMagicId :: ProtocolMagicId
mainnetProtocolMagicId = ProtocolMagicId 764824073
