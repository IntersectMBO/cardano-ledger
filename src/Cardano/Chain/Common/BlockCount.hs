{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Common.BlockCount
       ( BlockCount (..)
       ) where

import           Cardano.Prelude

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Formatting.Buildable (Buildable)

import           Cardano.Binary.Class (Bi (..))

newtype BlockCount = BlockCount
    { getBlockCount :: Word64
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
    encode = encode . getBlockCount
    decode = BlockCount <$> decode
    encodedSizeExpr size pxy = size (getBlockCount <$> pxy)

deriveJSON defaultOptions ''BlockCount
