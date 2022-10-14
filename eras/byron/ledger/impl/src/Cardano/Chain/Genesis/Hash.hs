{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Chain.Genesis.Hash
  ( GenesisHash (..),
  )
where

import Cardano.Crypto.Hashing (Hash)
import Cardano.Crypto.Raw (Raw)
import Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import Cardano.Prelude
import Data.Aeson (ToJSON)
import NoThunks.Class (NoThunks (..))

newtype GenesisHash = GenesisHash
  { unGenesisHash :: Hash Raw
  }
  deriving (Eq, Generic, NFData, FromCBOR, ToCBOR, NoThunks)

deriving instance Show GenesisHash

-- Used for debugging purposes only
instance ToJSON GenesisHash
