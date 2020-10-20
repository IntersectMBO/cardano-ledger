{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Coin
  ( Coin (..),
    CompactForm (..),
    DeltaCoin (..),
    word64ToCoin,
    coinToRational,
    rationalToCoinViaFloor,
    addDelta,
    toDelta,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Compactible
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Group (Abelian, Group (..))
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

-- | The amount of value held by a transaction output.
newtype Coin = Coin {unCoin :: Integer}
  deriving
    ( Eq,
      Ord,
      Enum,
      NoThunks,
      Generic,
      ToJSON,
      FromJSON,
      NFData
    )
  deriving (Show) via Quiet Coin
  deriving (ToCBOR, FromCBOR) via Compact Coin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd)

newtype DeltaCoin = DeltaCoin Integer
  deriving (Eq, Ord, Generic, Enum, NoThunks, NFData, FromCBOR, ToCBOR)
  deriving (Show) via Quiet DeltaCoin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd)

addDelta :: Coin -> DeltaCoin -> Coin
addDelta (Coin x) (DeltaCoin y) = Coin (x + y)

toDelta :: Coin -> DeltaCoin
toDelta (Coin x) = DeltaCoin x

word64ToCoin :: Word64 -> Coin
word64ToCoin w = Coin $ fromIntegral w

coinToRational :: Coin -> Rational
coinToRational (Coin c) = fromIntegral c

rationalToCoinViaFloor :: Rational -> Coin
rationalToCoinViaFloor r = Coin . floor $ r

-- FIXME:
-- if coin is less than 0 or greater than (maxBound :: Word64), then
-- fromIntegral constructs the incorrect value. for now this is handled
-- with an erroring bounds check here. where should this really live?
instance Compactible Coin where
  newtype CompactForm Coin = CompactCoin Word64
  toCompact (Coin c)
    | c < 0 = error $ "out of bounds : " ++ show c
    | c > (fromIntegral (maxBound :: Word64)) =
      error $ "out of bounds : " ++ show c
    | otherwise = CompactCoin (fromIntegral c)
  fromCompact (CompactCoin c) = word64ToCoin c

instance ToCBOR (CompactForm Coin) where
  toCBOR (CompactCoin c) = toCBOR c

instance FromCBOR (CompactForm Coin) where
  fromCBOR = CompactCoin <$> fromCBOR
