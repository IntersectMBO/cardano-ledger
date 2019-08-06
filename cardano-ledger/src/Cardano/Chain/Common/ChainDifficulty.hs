{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Common.ChainDifficulty
  ( ChainDifficulty(..)
  , dropChainDifficulty
  )
where

import Cardano.Prelude

import Formatting.Buildable (Buildable)

import Cardano.Binary
  (Dropper, FromCBOR(..), ToCBOR(..), dropWord64, encodeListLen, enforceSize)


-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
  { unChainDifficulty :: Word64
  } deriving (Show, Eq, Ord, Enum, Generic, Buildable, NFData)

instance ToCBOR ChainDifficulty where
    toCBOR cd = encodeListLen 1 <> toCBOR (unChainDifficulty cd)

instance FromCBOR ChainDifficulty where
    fromCBOR = do
        enforceSize "ChainDifficulty" 1
        ChainDifficulty <$> fromCBOR

dropChainDifficulty :: Dropper s
dropChainDifficulty = do
  enforceSize "ChainDifficulty" 1
  dropWord64

