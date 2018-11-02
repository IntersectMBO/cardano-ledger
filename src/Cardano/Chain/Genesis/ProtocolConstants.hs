{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Chain.Genesis.ProtocolConstants
       ( GenesisProtocolConstants (..)
       ) where

import           Cardano.Prelude

import           Data.Aeson.Options
    (defaultOptions)
import           Data.Aeson.TH
    (deriveJSON)

import           Cardano.Chain.ProtocolConstants
    (VssMaxTTL (..), VssMinTTL (..))
import           Cardano.Crypto
    (ProtocolMagic (..))

-- | 'GensisProtocolConstants' are not really part of genesis global state,
-- but they affect consensus, so they are part of 'GenesisSpec' and
-- 'GenesisData'.
data GenesisProtocolConstants = GenesisProtocolConstants
  { -- | Security parameter from the paper.
    gpcK             :: !Int
    -- | Magic constant for separating real/testnet.
  , gpcProtocolMagic :: !ProtocolMagic
    -- | VSS certificates max timeout to live (number of epochs).
  , gpcVssMaxTTL     :: !VssMaxTTL
    -- | VSS certificates min timeout to live (number of epochs).
  , gpcVssMinTTL     :: !VssMinTTL
  } deriving (Show, Eq, Generic)

deriveJSON defaultOptions ''GenesisProtocolConstants
