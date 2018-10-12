{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Common.ChainDifficulty
       ( ChainDifficulty (..)
       , dropChainDifficulty
       ) where

import           Cardano.Prelude

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Formatting.Buildable (Buildable)

import           Cardano.Binary.Class (Bi (..), Dropper, dropWord64,
                     encodeListLen, enforceSize)
import           Cardano.Chain.Common.BlockCount (BlockCount)

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
  { getChainDifficulty :: BlockCount
  } deriving ( Show
             , Eq
             , Ord
             , Num
             , Enum
             , Real
             , Integral
             , Generic
             , Buildable
             , Typeable
             , NFData
             )

instance Bi ChainDifficulty where
    encode cd = encodeListLen 1 <> encode (getChainDifficulty cd)
    decode = do
        enforceSize "ChainDifficulty" 1
        ChainDifficulty <$> decode

dropChainDifficulty :: Dropper s
dropChainDifficulty = do
  enforceSize "ChainDifficulty" 1
  dropWord64

deriveJSON defaultOptions ''ChainDifficulty
