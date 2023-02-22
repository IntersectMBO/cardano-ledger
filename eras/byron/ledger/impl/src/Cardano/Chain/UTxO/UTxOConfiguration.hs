{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.UTxO.UTxOConfiguration (
  UTxOConfiguration (..),
  defaultUTxOConfiguration,
  mkUTxOConfiguration,
)
where

import Cardano.Chain.Common.Address (Address)
import Cardano.Chain.Common.Compact (CompactAddress, toCompactAddress)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import qualified Data.Set as Set
import NoThunks.Class (NoThunks (..))

-- | Additional configuration for ledger validation.
data UTxOConfiguration = UTxOConfiguration
  { tcAssetLockedSrcAddrs :: !(Set CompactAddress)
  -- ^ Set of source address which are asset-locked. Transactions which
  -- use these addresses as transaction inputs will be deemed invalid.
  }
  deriving (Eq, Show, Generic, NoThunks)

instance ToCBOR UTxOConfiguration where
  toCBOR = toByronCBOR

instance FromCBOR UTxOConfiguration where
  fromCBOR = fromByronCBOR

instance EncCBOR UTxOConfiguration where
  encCBOR (UTxOConfiguration tcAssetLockedSrcAddrs_) =
    encodeListLen 1
      <> encCBOR @(Set CompactAddress) tcAssetLockedSrcAddrs_

instance DecCBOR UTxOConfiguration where
  decCBOR = do
    enforceSize "UTxOConfiguration" 1
    UTxOConfiguration <$> decCBOR @(Set CompactAddress)

defaultUTxOConfiguration :: UTxOConfiguration
defaultUTxOConfiguration =
  UTxOConfiguration
    { tcAssetLockedSrcAddrs = Set.empty
    }

mkUTxOConfiguration :: [Address] -> UTxOConfiguration
mkUTxOConfiguration lockedSrcAddrs =
  UTxOConfiguration
    { tcAssetLockedSrcAddrs = Set.fromList (map toCompactAddress lockedSrcAddrs)
    }
