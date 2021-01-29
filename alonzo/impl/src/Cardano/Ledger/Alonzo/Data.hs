{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Data
  ( PlutusData (..),
    -- Figure 2 (partial list)
    Data (Data, ..),
    DataHash,
    hashData,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeInt, encodeInt)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.SafeHash
  ( EraIndependentData,
    HashAnnotated,
    SafeHash,
    SafeToHash,
    hashAnnotated,
  )
import Data.Coders
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- =====================================================================
-- PlutusData is a placeholder for the type that Plutus expects as data.

data PlutusData = NotReallyData
  deriving (Eq, Show, Ord, Generic)

instance NoThunks PlutusData

-- | TODO appropriate serialisation for the Real Plutus Data
instance ToCBOR PlutusData where
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

newtype Data era = DataConstr (MemoBytes PlutusData)
  deriving (Eq, Ord, Generic, ToCBOR, Show)
  deriving newtype (SafeToHash)

deriving via
  (Mem PlutusData)
  instance
    (Era era) =>
    FromCBOR (Annotator (Data era))

instance (Crypto era ~ c) => HashAnnotated (Data era) EraIndependentData c

instance NoThunks (Data era)

pattern Data :: PlutusData -> Data era
pattern Data p <-
  DataConstr (Memo p _)
  where
    Data p = DataConstr (memoBytes (To p))

-- =============================================================================

type DataHash crypto = SafeHash crypto EraIndependentData

hashData :: Era era => Data era -> DataHash (Crypto era)
hashData d = hashAnnotated d
