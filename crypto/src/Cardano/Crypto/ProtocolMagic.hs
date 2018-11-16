{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Cardano.Crypto.ProtocolMagic
  ( ProtocolMagic(..)
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..))

-- | Magic number which should differ for different clusters. It's
--   defined here, because it's used for signing. It also used for other
--   things (e. g. it's part of a serialized block).
newtype ProtocolMagic = ProtocolMagic
  { getProtocolMagic :: Int32
  } deriving (Show, Eq, NFData)

deriving instance ToJSON ProtocolMagic
deriving instance FromJSON ProtocolMagic
