{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.ProtocolVersion (
  ProtocolVersion (..),
)
where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen, enforceSize)
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Formatting (bprint, shown)
import Formatting.Buildable (Buildable (..))
import NoThunks.Class (NoThunks (..))
import qualified Prelude

-- | Communication protocol version
data ProtocolVersion = ProtocolVersion
  { pvMajor :: !Word16
  , pvMinor :: !Word16
  , pvAlt :: !Word8
  }
  deriving (Eq, Generic, Ord)
  deriving anyclass (NFData, NoThunks)

instance Show ProtocolVersion where
  show pv =
    intercalate "." [show (pvMajor pv), show (pvMinor pv), show (pvAlt pv)]

instance Buildable ProtocolVersion where
  build = bprint shown

-- Used for debugging purposes only
instance ToJSON ProtocolVersion

instance EncCBOR ProtocolVersion where
  encCBOR pv =
    encodeListLen 3
      <> encCBOR (pvMajor pv)
      <> encCBOR (pvMinor pv)
      <> encCBOR
        (pvAlt pv)

  encodedSizeExpr f pv =
    1
      + encodedSizeExpr f (pvMajor <$> pv)
      + encodedSizeExpr f (pvMinor <$> pv)
      + encodedSizeExpr f (pvAlt <$> pv)

instance DecCBOR ProtocolVersion where
  decCBOR = do
    enforceSize "ProtocolVersion" 3
    ProtocolVersion <$> decCBOR <*> decCBOR <*> decCBOR
