{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Data
  ( Data (..),
    EraIndependentData,
    DataHash (..),
    hashData,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (HASH)
import Cardano.Ledger.Era (Crypto, Era)
import Control.DeepSeq (NFData)
import Data.Coders
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Hashing (HashAnnotated (..))

-- | TODO this should be isomorphic to the plutus type
data Data era = NotReallyData
  deriving (Eq, Ord, Show)

data EraIndependentData

newtype DataHash era
  = DataHash
      (Hash.Hash (HASH (Crypto era)) EraIndependentData)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

deriving newtype instance Era era => FromCBOR (DataHash era)

deriving newtype instance Era era => ToCBOR (DataHash era)

instance Era era => HashAnnotated (Data era) era where
  type HashIndex (Data era) = EraIndependentData

hashData :: Era era => Data era -> DataHash era
hashData = DataHash . hashAnnotated

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | TODO appropriate serialisation
instance Era era => ToCBOR (Data era) where
  toCBOR = encode . encodeData
    where
      encodeData NotReallyData = Sum NotReallyData 0

instance Era era => FromCBOR (Data era) where
  fromCBOR = decode $ Summands "Data" decodeData
    where
      decodeData 0 = SumD NotReallyData
      decodeData n = Invalid n
