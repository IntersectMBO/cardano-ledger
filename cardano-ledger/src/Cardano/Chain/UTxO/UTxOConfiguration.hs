{-# LANGUAGE DeriveGeneric #-}

module Cardano.Chain.UTxO.UTxOConfiguration
  ( UTxOConfiguration(..)
  , defaultUTxOConfiguration
  , mkUTxOConfiguration
  )
where

import Cardano.Prelude

import qualified Data.Set as Set

import Cardano.Chain.Common.Address (Address)
import Cardano.Chain.Common.Compact (CompactAddress, toCompactAddress)


-- | Additional configuration for ledger validation.
data UTxOConfiguration = UTxOConfiguration
  { -- | Set of source address which are asset-locked. Transactions which
    -- use these addresses as transaction inputs will be deemed invalid.
    tcAssetLockedSrcAddrs :: !(Set CompactAddress)
  } deriving (Eq,Show,Generic)

defaultUTxOConfiguration :: UTxOConfiguration
defaultUTxOConfiguration =
    UTxOConfiguration {
      tcAssetLockedSrcAddrs = Set.empty
    }

mkUTxOConfiguration :: [Address] -> UTxOConfiguration
mkUTxOConfiguration lockedSrcAddrs =
    UTxOConfiguration {
      tcAssetLockedSrcAddrs = Set.fromList (map toCompactAddress lockedSrcAddrs)
    }
