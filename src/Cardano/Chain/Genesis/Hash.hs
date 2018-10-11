{-# LANGUAGE Rank2Types #-}

module Cardano.Chain.Genesis.Hash
       ( GenesisHash (..)
       ) where

import           Cardano.Crypto.Hashing (Hash)

newtype GenesisHash = GenesisHash { getGenesisHash :: forall a . Hash a }
