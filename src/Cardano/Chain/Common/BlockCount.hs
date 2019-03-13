{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Common.BlockCount
  ( BlockCount(..)
  )
where

import Cardano.Prelude

import Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import Formatting.Buildable (Buildable)

import Cardano.Binary.Class (Bi(..))

import Cardano.Chain.Common.BlockCount.JSON (unToGet)

newtype BlockCount = BlockCount
    { unBlockCount :: Word64
    } deriving ( Eq
              , Ord
              , Num
              , Real
              , Integral
              , Enum
              , Read
              , Show
              , Buildable
              , Generic
              , NFData
              )

instance Bi BlockCount where
    encode = encode . unBlockCount
    decode = BlockCount <$> decode
    encodedSizeExpr size pxy = size (unBlockCount <$> pxy)

deriveJSON (defaultOptions { fieldLabelModifier = unToGet }) ''BlockCount
