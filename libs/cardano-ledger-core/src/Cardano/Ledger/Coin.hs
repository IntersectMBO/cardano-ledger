{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Cardano.Ledger.Coin (
  Coin (..),
  CompactForm (..),
  DeltaCoin (..),
  word64ToCoin,
  coinToRational,
  rationalToCoinViaFloor,
  rationalToCoinViaCeiling,
  addDeltaCoin,
  toDeltaCoin,
  integerToWord64,
  decodePositiveCoin,
)
where

import Cardano.HeapWords (HeapWords)
import Cardano.Ledger.Binary (
  Decoder,
  FromCBOR (..),
  ToCBOR (..),
  decodeInteger,
  decodeWord64,
  ifDecoderVersionAtLeast,
  natVersion,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.TreeDiff (ToExpr (toExpr))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Group (Abelian, Group (..))
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Primitive.Types
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

-- | The amount of value held by a transaction output.
newtype Coin = Coin {unCoin :: Integer}
  deriving
    ( Eq
    , Ord
    , Enum
    , NoThunks
    , Generic
    , ToJSON
    , FromJSON
    , NFData
    )
  deriving (Show) via Quiet Coin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd, ToCBOR, HeapWords)

instance FromCBOR Coin where
  fromCBOR =
    ifDecoderVersionAtLeast
      (natVersion @9)
      (Coin . fromIntegral <$> decodeWord64)
      (Coin <$> decodeInteger)

newtype DeltaCoin = DeltaCoin Integer
  deriving (Eq, Ord, Generic, Enum, NoThunks, NFData, FromCBOR, ToCBOR, HeapWords)
  deriving (Show) via Quiet DeltaCoin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd)

addDeltaCoin :: Coin -> DeltaCoin -> Coin
addDeltaCoin (Coin x) (DeltaCoin y) = Coin (x + y)

toDeltaCoin :: Coin -> DeltaCoin
toDeltaCoin (Coin x) = DeltaCoin x

word64ToCoin :: Word64 -> Coin
word64ToCoin = Coin . fromIntegral

coinToRational :: Coin -> Rational
coinToRational (Coin c) = fromIntegral c

rationalToCoinViaFloor :: Rational -> Coin
rationalToCoinViaFloor = Coin . floor

rationalToCoinViaCeiling :: Rational -> Coin
rationalToCoinViaCeiling = Coin . ceiling

instance Compactible Coin where
  newtype CompactForm Coin = CompactCoin Word64
    deriving (Eq, Show, NoThunks, NFData, Typeable, HeapWords, Prim, Ord)
    deriving (Semigroup, Monoid, Group, Abelian) via Sum Word64

  toCompact (Coin c) = CompactCoin <$> integerToWord64 c
  fromCompact (CompactCoin c) = word64ToCoin c

instance Compactible DeltaCoin where
  newtype CompactForm DeltaCoin = CompactDeltaCoin Word64
    deriving (Eq, Show, NoThunks, NFData, Typeable, HeapWords, Prim)

  toCompact (DeltaCoin dc) = CompactDeltaCoin <$> integerToWord64 dc
  fromCompact (CompactDeltaCoin cdc) = DeltaCoin (unCoin (word64ToCoin cdc))

-- It's odd for this to live here. Where should it go?
integerToWord64 :: Integer -> Maybe Word64
integerToWord64 c
  | c < 0 = Nothing
  | c > fromIntegral (maxBound :: Word64) = Nothing
  | otherwise = Just $ fromIntegral c
{-# INLINE integerToWord64 #-}

instance ToCBOR (CompactForm Coin) where
  toCBOR (CompactCoin c) = toCBOR c

instance FromCBOR (CompactForm Coin) where
  fromCBOR = CompactCoin <$> fromCBOR

instance ToCBOR (CompactForm DeltaCoin) where
  toCBOR (CompactDeltaCoin c) = toCBOR c

instance FromCBOR (CompactForm DeltaCoin) where
  fromCBOR = CompactDeltaCoin <$> fromCBOR

-- ================================

instance ToExpr Coin

instance ToExpr DeltaCoin

instance ToExpr (CompactForm Coin) where
  toExpr x = toExpr (fromCompact x)

decodePositiveCoin :: Decoder s Coin
decodePositiveCoin = do
  n <- decodeWord64
  if n == 0
    then fail "Expected a positive Coin. Got 0 (zero)."
    else pure $ Coin (toInteger n)
