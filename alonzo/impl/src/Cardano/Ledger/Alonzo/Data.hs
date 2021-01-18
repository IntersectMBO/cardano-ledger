{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Data
  ( PlutusData (..),
    -- Figure 2 (partial list)
    Data (Data, ..),
    EraIndependentData,
    DataHash (..),
    hashData,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeInt, encodeInt)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Control.DeepSeq (NFData)
import Data.Coders
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Hashing (HashAnnotated (..))

-- =====================================================================
-- PlutusData is a placeholder for the type that Plutus expects as data.

data PlutusData = NotReallyData
  deriving (Eq, Show, Ord, Generic)

instance NoThunks PlutusData

-- | TODO appropriate serialisation for the Real Plutus Data
instance ToCBOR (PlutusData) where
  toCBOR _ = encodeInt 0

instance FromCBOR (PlutusData) where
  fromCBOR = do
    i <- decodeInt
    case i of
      0 -> pure NotReallyData
      _ -> fail "oh no"

instance FromCBOR (Annotator PlutusData) where
  fromCBOR = pure <$> fromCBOR

-- ============================================================================
-- the newtype Data is a wrapper around the type that Plutus expects as data.
-- The newtype will memoize the serialized bytes. The strategy is to replace
-- PlutusData  with the correct type

newtype Data era = DataConstr (MemoBytes (PlutusData))
  deriving (Eq, Ord, Generic, ToCBOR, Show)

deriving via
  (Mem PlutusData)
  instance
    (Era era) =>
    FromCBOR (Annotator (Data era))

instance NoThunks (Data era)

pattern Data :: PlutusData -> Data era
pattern Data p <-
  DataConstr (Memo p _)
  where
    Data p = DataConstr (memoBytes (To p))

-- =============================================================================

data EraIndependentData

newtype DataHash crypto
  = DataHash
      (Hash.Hash (HASH crypto) EraIndependentData)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

deriving newtype instance CC.Crypto crypto => FromCBOR (DataHash crypto)

deriving newtype instance CC.Crypto crypto => ToCBOR (DataHash crypto)

instance Era era => HashAnnotated (Data era) era where
  type HashIndex (Data era) = EraIndependentData

hashData :: Era era => Data era -> DataHash (Crypto era)
hashData = DataHash . hashAnnotated

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------
