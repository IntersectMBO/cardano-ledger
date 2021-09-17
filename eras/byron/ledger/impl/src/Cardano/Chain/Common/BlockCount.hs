{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Chain.Common.BlockCount
  ( BlockCount (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Prelude
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))

newtype BlockCount = BlockCount
  { unBlockCount :: Word64
  }
  deriving (Eq, Ord, Enum, Read, Show, Buildable, Generic, NFData, NoThunks)

instance ToCBOR BlockCount where
  toCBOR = toCBOR . unBlockCount
  encodedSizeExpr size pxy = size (unBlockCount <$> pxy)

instance FromCBOR BlockCount where
  fromCBOR = BlockCount <$> fromCBOR
