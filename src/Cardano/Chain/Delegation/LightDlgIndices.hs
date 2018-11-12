{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Delegation.LightDlgIndices
  ( LightDlgIndices(..)
  , ProxySigLight
  , ProxySKLight
  )
where

import Cardano.Prelude

import Formatting (bprint, build)
import qualified Formatting.Buildable as B (Buildable(..))

import Cardano.Binary.Class (Bi(..))
import Cardano.Chain.Slotting (EpochIndex)
import Cardano.Crypto (ProxySecretKey(..), ProxySignature)

-- Notice: light delegation was removed as part of CSL-1856 and should be
-- reworked later. Though some parts of it are left to support backward
-- compatibility.

-- | Pair of indices for light delegation PSK that define start and end epoch of
--   cert usage. Block is valid if its epoch index is inside this range.
newtype LightDlgIndices = LightDlgIndices
  { getLightDlgIndices :: (EpochIndex, EpochIndex)
  } deriving (Show, Eq, Ord, Generic)
    deriving anyclass NFData

instance B.Buildable LightDlgIndices where
  build (LightDlgIndices (a, b)) = bprint ("(" . build . ", " . build . ")") a b

instance Bi LightDlgIndices where
  encode = encode . getLightDlgIndices
  decode = LightDlgIndices <$> decode

-- | Light delegation proxy signature, that holds a pair of epoch indices
type ProxySigLight a = ProxySignature LightDlgIndices a

-- | Same alias for the proxy secret key (see 'ProxySigLight')
type ProxySKLight = ProxySecretKey LightDlgIndices
