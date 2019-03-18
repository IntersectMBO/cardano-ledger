{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Chain.Genesis.Hash
  ( GenesisHash(..)
  )
where

import Cardano.Prelude

import Cardano.Binary.Class (Raw)
import Cardano.Crypto.Hashing (Hash)


newtype GenesisHash = GenesisHash
  { unGenesisHash :: Hash Raw
  }

deriving instance Show GenesisHash
