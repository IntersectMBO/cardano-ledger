{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Common.BlockCount
  ( BlockCount(..)
  )
where

import Cardano.Prelude

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Formatting.Buildable (Buildable)

import Cardano.Binary (FromCBOR(..), ToCBOR(..))


newtype BlockCount = BlockCount
  { unBlockCount :: Word64
  } deriving ( Eq, Ord, Enum, Read, Show, Buildable, Generic, NFData)

instance ToCBOR BlockCount where
  toCBOR = toCBOR . unBlockCount
  encodedSizeExpr size pxy = size (unBlockCount <$> pxy)

instance FromCBOR BlockCount where
  fromCBOR = BlockCount <$> fromCBOR

deriveJSON defaultOptions ''BlockCount
