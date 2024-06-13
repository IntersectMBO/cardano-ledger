{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  fromDeltaCoin,
  integerToWord64,
  decodePositiveCoin,
  compactCoinOrError,
)
where

import Cardano.HeapWords (HeapWords)
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR,
  decodeInteger,
  decodeWord64,
  ifDecoderVersionAtLeast,
  natVersion,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Compactible
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Group (Abelian, Group (..))
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Primitive.Types
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Stack
import NoThunks.Class (NoThunks (..))
import Quiet
import System.Random.Stateful (Uniform (..), UniformRange (..))

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
  deriving newtype (PartialOrd, ToCBOR, EncCBOR, HeapWords)

instance FromCBOR Coin where
  fromCBOR = Coin . toInteger <$> Plain.decodeWord64

instance DecCBOR Coin where
  decCBOR =
    ifDecoderVersionAtLeast
      (natVersion @9)
      (Coin . fromIntegral <$> decodeWord64)
      (Coin <$> decodeInteger)

newtype DeltaCoin = DeltaCoin Integer
  deriving (Eq, Ord, Generic, Enum, NoThunks, HeapWords)
  deriving (Show) via Quiet DeltaCoin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd, NFData, ToCBOR, DecCBOR, EncCBOR, ToJSON, FromJSON)

instance Inject Coin DeltaCoin where
  inject = coerce

addDeltaCoin :: Coin -> DeltaCoin -> Coin
addDeltaCoin (Coin x) (DeltaCoin y) = Coin (x + y)

toDeltaCoin :: Coin -> DeltaCoin
toDeltaCoin (Coin x) = DeltaCoin x

fromDeltaCoin :: DeltaCoin -> Maybe Coin
fromDeltaCoin (DeltaCoin x)
  | x < 0 = Nothing
  | otherwise = Just $ Coin x

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
    deriving (Eq, Show, NoThunks, NFData, Typeable, HeapWords, Prim, Ord, ToCBOR, ToJSON, FromJSON)
    deriving (Semigroup, Monoid, Group, Abelian) via Sum Word64

  toCompact (Coin c) = CompactCoin <$> integerToWord64 c
  fromCompact (CompactCoin c) = word64ToCoin c

instance Compactible DeltaCoin where
  newtype CompactForm DeltaCoin = CompactDeltaCoin Word64
    deriving (Eq, Show, NoThunks, NFData, Typeable, HeapWords, ToJSON, Prim)

  toCompact (DeltaCoin dc) = CompactDeltaCoin <$> integerToWord64 dc
  fromCompact (CompactDeltaCoin cdc) = DeltaCoin (unCoin (word64ToCoin cdc))

-- It's odd for this to live here. Where should it go?
integerToWord64 :: Integer -> Maybe Word64
integerToWord64 c
  | c < 0 = Nothing
  | c > fromIntegral (maxBound :: Word64) = Nothing
  | otherwise = Just $ fromIntegral c
{-# INLINE integerToWord64 #-}

compactCoinOrError :: HasCallStack => Coin -> CompactForm Coin
compactCoinOrError c =
  case toCompact c of
    Nothing -> error $ "Invalid ADA value: " <> show c
    Just compactCoin -> compactCoin

instance EncCBOR (CompactForm Coin) where
  encCBOR (CompactCoin c) = encCBOR c

instance DecCBOR (CompactForm Coin) where
  decCBOR = CompactCoin <$> decCBOR

instance EncCBOR (CompactForm DeltaCoin) where
  encCBOR (CompactDeltaCoin c) = encCBOR c

instance DecCBOR (CompactForm DeltaCoin) where
  decCBOR = CompactDeltaCoin <$> decCBOR

-- ================================

decodePositiveCoin :: String -> Decoder s Coin
decodePositiveCoin errorMessage = do
  n <- decodeWord64
  if n == 0
    then fail $ errorMessage ++ ": Expected a positive Coin. Got 0 (zero)."
    else pure $ Coin (toInteger n)

instance Uniform Coin where
  uniformM g = fromCompact <$> uniformM g

instance Uniform (CompactForm Coin) where
  uniformM g = CompactCoin <$> uniformM g

instance UniformRange Coin where
  uniformRM (l, h) g = fromCompact <$> uniformRM (compactCoinOrError l, compactCoinOrError h) g

instance UniformRange (CompactForm Coin) where
  uniformRM (CompactCoin l, CompactCoin h) g = CompactCoin <$> uniformRM (l, h) g
