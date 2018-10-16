{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Chain.Ssc.Opening
       ( Opening (..)
       ) where

import           Cardano.Prelude

import           Formatting.Buildable (Buildable)

import           Cardano.Binary.Class (AsBinary, Bi (..))
import           Cardano.Crypto (Secret)

-- | Opening reveals secret.
newtype Opening = Opening
    { getOpening :: AsBinary Secret
    } deriving (Show, Eq, Generic, Buildable, NFData)

instance Bi Opening where
    encode = encode . getOpening
    decode = Opening <$> decode
