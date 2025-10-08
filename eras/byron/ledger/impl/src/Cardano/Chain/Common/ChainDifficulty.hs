{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Common.ChainDifficulty (
  ChainDifficulty (..),
  dropChainDifficulty,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  Dropper,
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  dropWord64,
  enforceSize,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain (
  encodeListLen,
  enforceSize,
 )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
  { unChainDifficulty :: Word64
  }
  deriving (Show, Eq, Ord, Enum, Generic, Buildable, NFData, NoThunks)

-- Used for debugging purposes only
instance ToJSON ChainDifficulty

instance ToCBOR ChainDifficulty where
  toCBOR cd = Plain.encodeListLen 1 <> toCBOR (unChainDifficulty cd)
  encodedSizeExpr f cd = 1 + encodedSizeExpr f (unChainDifficulty <$> cd)

instance FromCBOR ChainDifficulty where
  fromCBOR = do
    Plain.enforceSize "ChainDifficulty" 1
    ChainDifficulty <$> fromCBOR

instance EncCBOR ChainDifficulty

instance DecCBOR ChainDifficulty

dropChainDifficulty :: Dropper s
dropChainDifficulty = do
  enforceSize "ChainDifficulty" 1
  dropWord64
