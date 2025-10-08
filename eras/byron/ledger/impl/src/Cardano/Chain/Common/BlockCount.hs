{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Chain.Common.BlockCount (
  BlockCount (..),
) where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR, FromCBOR, ToCBOR (..))
import Cardano.Prelude
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))

newtype BlockCount = BlockCount
  { unBlockCount :: Word64
  }
  deriving (Eq, Ord, Enum, Read, Show, Buildable, Generic, NFData, NoThunks, FromCBOR)

instance ToCBOR BlockCount where
  toCBOR = toCBOR . unBlockCount
  encodedSizeExpr size pxy = size (unBlockCount <$> pxy)

instance EncCBOR BlockCount

instance DecCBOR BlockCount
