{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Chain.Common.BlockCount (
  BlockCount (..),
)
where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))

newtype BlockCount = BlockCount
  { unBlockCount :: Word64
  }
  deriving (Eq, Ord, Enum, Read, Show, Buildable, Generic, NFData, NoThunks)

instance ToCBOR BlockCount where
  toCBOR = toByronCBOR

instance FromCBOR BlockCount where
  fromCBOR = fromByronCBOR

instance EncCBOR BlockCount where
  encCBOR = encCBOR . unBlockCount
  encodedSizeExpr size pxy = size (unBlockCount <$> pxy)

instance DecCBOR BlockCount where
  decCBOR = BlockCount <$> decCBOR
