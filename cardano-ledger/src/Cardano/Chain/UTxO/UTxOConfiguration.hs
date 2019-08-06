{-# LANGUAGE DeriveGeneric #-}

module Cardano.Chain.UTxO.UTxOConfiguration
  ( UTxOConfiguration(..)
  )
where

import Cardano.Prelude

import Cardano.Chain.Common.Address (Address)

-- | Additional configuration for ledger validation.
data UTxOConfiguration = UTxOConfiguration
  { -- | Set of source address which are asset-locked. Transactions which
    -- use these addresses as transaction inputs will be silently dropped.
    tcAssetLockedSrcAddrs :: !(Set Address)
  } deriving (Eq,Show,Generic)

