{-# LANGUAGE DeriveGeneric #-}

module Cardano.Chain.UTxO.UTxOConfiguration
  ( UTxOConfiguration(..)
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Aeson.Options (defaultOptions)

import Cardano.Chain.Common.Address (Address)

-- | Delegation configruation part.
data UTxOConfiguration = UTxOConfiguration
  { -- | Limit on the number of transactions that can be stored in
    -- the mem pool.
    ccMemPoolLimitTx      :: !Int

    -- | Set of source address which are asset-locked. Transactions which
    -- use these addresses as transaction inputs will be silently dropped.
  , tcAssetLockedSrcAddrs :: !(Set Address)
  } deriving (Eq,Show,Generic)

instance ToJSON UTxOConfiguration where
  toJSON = genericToJSON defaultOptions

instance FromJSON UTxOConfiguration where
  parseJSON = genericParseJSON defaultOptions
