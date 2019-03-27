{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Common.NetworkMagic
       ( NetworkMagic (..)
       , makeNetworkMagic
       ) where

import Cardano.Prelude hiding ((%))

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as B

import           Cardano.Crypto.ProtocolMagic (ProtocolMagic (..),
                     RequiresNetworkMagic (..), getProtocolMagic)


--------------------------------------------------------------------------------
-- NetworkMagic
--------------------------------------------------------------------------------

-- Although it doesn't make sense for an identifier to have a sign, we're opting
-- to maintain consistency with `ProtocolMagicId`, rather than risk subtle
-- conversion bugs.
data NetworkMagic
    = NetworkMainOrStage
    | NetworkTestnet {-# UNPACK #-} !Int32
    deriving (Show, Eq, Ord, Generic, NFData)

instance B.Buildable NetworkMagic where
    build NetworkMainOrStage = "NetworkMainOrStage"
    build (NetworkTestnet n) = bprint ("NetworkTestnet ("%build%")") n

instance HeapWords NetworkMagic where
  heapWords NetworkMainOrStage = 0
  heapWords (NetworkTestnet _) = 2

makeNetworkMagic :: ProtocolMagic -> NetworkMagic
makeNetworkMagic pm = case getRequiresNetworkMagic pm of
    RequiresNoMagic -> NetworkMainOrStage
    RequiresMagic   -> NetworkTestnet (getProtocolMagic pm)
