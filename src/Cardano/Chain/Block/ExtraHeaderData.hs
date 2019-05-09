{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Block.ExtraHeaderData
  ( ExtraHeaderData(..)
  )
where

import Cardano.Prelude

import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary
  (FromCBOR(..), ToCBOR(..), dropBytes, encodeListLen, enforceSize)
import Cardano.Chain.Common (dropEmptyAttributes)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto (hashRaw)


-- | Represents block header extra data
data ExtraHeaderData = ExtraHeaderData
  { ehdProtocolVersion :: !ProtocolVersion
  -- ^ Protocol version used by this block
  , ehdSoftwareVersion :: !SoftwareVersion
  -- ^ Software version used by this block
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance B.Buildable ExtraHeaderData where
  build mehd = bprint
    ("    block: v" . build . "\n" . "    software: " . build)
    (ehdProtocolVersion mehd)
    (ehdSoftwareVersion mehd)

instance ToCBOR ExtraHeaderData where
  toCBOR ehd =
    encodeListLen 4
      <> toCBOR (ehdProtocolVersion ehd)
      <> toCBOR (ehdSoftwareVersion ehd)
      -- Encoding of empty Attributes
      <> toCBOR (mempty :: Map Word8 LByteString)
      -- Hash of the encoding of empty ExtraBodyData
      <> toCBOR (hashRaw "\129\160")

instance FromCBOR ExtraHeaderData where
  fromCBOR = do
    enforceSize "ExtraHeaderData" 4
    ExtraHeaderData
      <$> fromCBOR
      <*> fromCBOR
      <*  dropEmptyAttributes
      <*  dropBytes
