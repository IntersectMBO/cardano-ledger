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

import Cardano.Binary (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
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

instance ToCBOR ExtraBodyData where
  toCBOR ebd = encodeListLen 1 <> toCBOR (ebdAttributes ebd)

instance FromCBOR ExtraBodyData where
  fromCBOR = do
    enforceSize "ExtraBodyData" 1
    ExtraBodyData <$> fromCBOR
