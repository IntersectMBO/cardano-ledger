{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Common.Compact (
  CompactAddress,
  toCompactAddress,
  fromCompactAddress,
  unsafeGetCompactAddress,
)
where

import Cardano.Chain.Common.Address (Address (..))
import Cardano.HeapWords (HeapWords)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), byronProtVer, decodeFull', serialize')
import Cardano.Prelude
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS (fromShort, toShort)
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Compact Address
--------------------------------------------------------------------------------

-- | A compact in-memory representation for an 'Address'.
--
-- Convert using 'toCompactAddress' and 'fromCompactAddress'.
newtype CompactAddress = CompactAddress ShortByteString
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (HeapWords, NoThunks)
  deriving anyclass (NFData)

instance DecCBOR CompactAddress where
  decCBOR = CompactAddress . BSS.toShort <$> decCBOR

instance EncCBOR CompactAddress where
  encCBOR (CompactAddress sbs) = encCBOR (BSS.fromShort sbs)

toCompactAddress :: Address -> CompactAddress
toCompactAddress addr =
  CompactAddress (BSS.toShort (serialize' byronProtVer addr))

fromCompactAddress :: CompactAddress -> Address
fromCompactAddress (CompactAddress addr) =
  case decodeFull' byronProtVer (BSS.fromShort addr) of
    Left err -> panic ("fromCompactAddress: impossible: " <> show err)
    Right decAddr -> decAddr

unsafeGetCompactAddress :: CompactAddress -> ShortByteString
unsafeGetCompactAddress (CompactAddress sbs) = sbs
