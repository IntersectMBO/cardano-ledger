{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Common.Compact
  ( CompactAddress
  , toCompactAddress
  , fromCompactAddress
  )
where

import Cardano.Prelude

import Cardano.Binary (serialize', decodeFull')
import qualified Data.ByteString.Short as BSS (fromShort, toShort)
import Data.ByteString.Short (ShortByteString)

import Cardano.Chain.Common.Address (Address(..))

--------------------------------------------------------------------------------
-- Compact Address
--------------------------------------------------------------------------------

-- | A compact in-memory representation for an 'Address'.
--
-- Convert using 'toCompactAddress' and 'fromCompactAddress'.
--
newtype CompactAddress = CompactAddress ShortByteString
  deriving (Eq, Ord, Generic, Show)
  deriving newtype HeapWords
  deriving anyclass NFData

toCompactAddress :: Address -> CompactAddress
toCompactAddress addr =
  CompactAddress (BSS.toShort (serialize' addr))

fromCompactAddress :: CompactAddress -> Address
fromCompactAddress (CompactAddress addr) =
  case decodeFull' (BSS.fromShort addr) of
    Left err      -> panic ("fromCompactAddress: impossible: " <> show err)
    Right decAddr -> decAddr
