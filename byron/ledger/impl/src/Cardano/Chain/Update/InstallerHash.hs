{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.InstallerHash
  ( InstallerHash (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    Raw,
    ToCBOR (..),
    dropBytes,
    encodeListLen,
    enforceSize,
  )
import Cardano.Crypto (Hash, hashRaw)
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

-- Used for debugging purposes only
instance ToJSON InstallerHash

instance ToCBOR InstallerHash where
  toCBOR (InstallerHash h) =
    encodeListLen 4
      <> toCBOR emptyHash
      <> toCBOR h
      <> toCBOR emptyHash
      <> toCBOR emptyHash
    where
      emptyHash = hashRaw "\NUL"

instance FromCBOR InstallerHash where
  fromCBOR = do
    enforceSize "InstallerHash" 4
    dropBytes
    h <- fromCBOR
    dropBytes
    dropBytes
    pure $ InstallerHash h
