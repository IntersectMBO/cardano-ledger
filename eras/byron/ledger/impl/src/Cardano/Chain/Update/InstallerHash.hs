{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.InstallerHash (
  InstallerHash (..),
)
where

import Cardano.Crypto (Hash, hashRaw)
import Cardano.Crypto.Raw (Raw)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  dropBytes,
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

-- | The hash of the installer of the new application
newtype InstallerHash = InstallerHash
  { unInstallerHash :: Hash Raw
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance B.Buildable InstallerHash where
  build (InstallerHash h) = bprint ("{ installer hash: " . build . " }") h

instance ToCBOR InstallerHash where
  toCBOR = toByronCBOR

instance FromCBOR InstallerHash where
  fromCBOR = fromByronCBOR

-- Used for debugging purposes only
instance ToJSON InstallerHash

instance EncCBOR InstallerHash where
  encCBOR (InstallerHash h) =
    encodeListLen 4
      <> encCBOR emptyHash
      <> encCBOR h
      <> encCBOR emptyHash
      <> encCBOR emptyHash
    where
      emptyHash = hashRaw "\NUL"

instance DecCBOR InstallerHash where
  decCBOR = do
    enforceSize "InstallerHash" 4
    dropBytes
    h <- decCBOR
    dropBytes
    dropBytes
    pure $ InstallerHash h
