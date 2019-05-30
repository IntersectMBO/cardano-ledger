{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cardano.Chain.Update.ProtocolVersion
  ( ProtocolVersion(..)
  )
where

import Cardano.Prelude

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Formatting (bprint, shown)
import Formatting.Buildable (Buildable(..))
import qualified Prelude

import Cardano.Binary (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)


-- | Communication protocol version
data ProtocolVersion = ProtocolVersion
  { pvMajor :: !Word16
  , pvMinor :: !Word16
  , pvAlt   :: !Word8
  } deriving (Eq, Generic, Ord)
    deriving anyclass NFData

instance Show ProtocolVersion where
  show pv =
    intercalate "." [show (pvMajor pv), show (pvMinor pv), show (pvAlt pv)]

instance Buildable ProtocolVersion where
  build = bprint shown

instance ToCBOR ProtocolVersion where
  toCBOR pv =
    encodeListLen 3 <> toCBOR (pvMajor pv) <> toCBOR (pvMinor pv) <> toCBOR
      (pvAlt pv)

instance FromCBOR ProtocolVersion where
  fromCBOR = do
    enforceSize "ProtocolVersion" 3
    ProtocolVersion <$> fromCBOR <*> fromCBOR <*> fromCBOR

deriveJSON defaultOptions ''ProtocolVersion
