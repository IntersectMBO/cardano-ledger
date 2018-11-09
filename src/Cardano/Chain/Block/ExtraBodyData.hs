{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Block.ExtraBodyData
  ( ExtraBodyData(..)
  )
where

import Cardano.Prelude

import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Common (Attributes, areAttributesKnown)


-- | Represents main block extra data
newtype ExtraBodyData = ExtraBodyData
  { ebdAttributes :: Attributes ()
  } deriving (Eq, Show, Generic)
    deriving newtype NFData

instance B.Buildable ExtraBodyData where
  build (ExtraBodyData attrs)
    | areAttributesKnown attrs = "no extra data"
    | otherwise = bprint ("extra data has attributes: " . build) attrs

instance Bi ExtraBodyData where
  encode ebd = encodeListLen 1 <> encode (ebdAttributes ebd)
  decode = do
    enforceSize "ExtraBodyData" 1
    ExtraBodyData <$> decode
