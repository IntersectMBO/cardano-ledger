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

import           Cardano.Crypto.ProtocolMagic (AProtocolMagic (..),
                     RequiresNetworkMagic (..), getProtocolMagic)


--------------------------------------------------------------------------------
-- NetworkMagic
--------------------------------------------------------------------------------

data NetworkMagic
    = NetworkMainOrStage
    | NetworkTestnet {-# UNPACK #-} !Word32
    deriving (Show, Eq, Ord, Generic, NFData)

instance B.Buildable NetworkMagic where
    build NetworkMainOrStage = "NetworkMainOrStage"
    build (NetworkTestnet n) = bprint ("NetworkTestnet ("%build%")") n

instance HeapWords NetworkMagic where
  heapWords NetworkMainOrStage = 0
  heapWords (NetworkTestnet _) = 2

makeNetworkMagic :: AProtocolMagic a -> NetworkMagic
makeNetworkMagic pm = case getRequiresNetworkMagic pm of
    RequiresNoMagic -> NetworkMainOrStage
    RequiresMagic   -> NetworkTestnet (getProtocolMagic pm)
