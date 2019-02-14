{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Chain.Genesis.Hash
  ( GenesisHash(..)
  )
where

import Cardano.Prelude

import Cardano.Crypto.Hashing (Hash)


newtype GenesisHash = GenesisHash
  { unGenesisHash :: forall a. Hash a
  }

deriving instance Show GenesisHash
