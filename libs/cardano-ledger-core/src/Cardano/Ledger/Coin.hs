{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
  addCompactCoin,
  sumCompactCoin,
  partialCompactCoinL,
  -- NonZero helpers
  toCompactCoinNonZero,
  unCoinNonZero,
  toCoinNonZero,
  fromCompactCoinNonZero,
  compactCoinNonZero,
) where

import Cardano.Ledger.BaseTypes (
  HasZero (..),
  Inject (..),
  NonZero,
  unNonZero,
  unsafeNonZero,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR,
  decodeWord64,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Compactible
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import qualified Data.Foldable as F (foldl') -- Drop this when ghc >= 9.10
import Data.Group (Abelian, Group (..))
import Data.MemPack
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Primitive.Types
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro (Lens', lens)
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
  deriving newtype (PartialOrd, ToCBOR, EncCBOR)

instance FromCBOR Coin where
  fromCBOR = Coin . toInteger <$> Plain.decodeWord64

instance DecCBOR Coin

newtype DeltaCoin = DeltaCoin Integer
  deriving (Eq, Ord, Generic, Enum, NoThunks)
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
  newtype CompactForm Coin = CompactCoin {unCompactCoin :: Word64}
    deriving (Eq, Show, NoThunks, NFData, Prim, Ord, ToCBOR, ToJSON, FromJSON, Generic)
    deriving (Semigroup, Monoid, Group, Abelian) via Sum Word64

  toCompact (Coin c) = CompactCoin <$> integerToWord64 c
  fromCompact (CompactCoin c) = word64ToCoin c

-- | This instance prefixes with a 0 Tag for binary compatibility with compact form of multiassets.
instance MemPack (CompactForm Coin) where
  packedByteCount (CompactCoin c) =
    packedTagByteCount + packedByteCount (VarLen c)
  {-# INLINE packedByteCount #-}
  packM (CompactCoin c) = packTagM 0 >> packM (VarLen c)
  {-# INLINE packM #-}
  unpackM = do
    unpackTagM >>= \case
      0 -> CompactCoin . unVarLen <$> unpackM
      n -> unknownTagM @(CompactForm Coin) n
  {-# INLINE unpackM #-}

instance Compactible DeltaCoin where
  newtype CompactForm DeltaCoin = CompactDeltaCoin Word64
    deriving (Eq, Show, NoThunks, NFData, ToJSON, Prim)

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

addCompactCoin :: CompactForm Coin -> CompactForm Coin -> CompactForm Coin
addCompactCoin (CompactCoin x) (CompactCoin y) = CompactCoin (x + y)

sumCompactCoin :: Foldable t => t (CompactForm Coin) -> CompactForm Coin
sumCompactCoin = F.foldl' addCompactCoin (CompactCoin 0)

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

instance HasZero Coin where
  isZero = (== Coin 0)

toCompactCoinNonZero :: NonZero Coin -> Maybe (NonZero (CompactForm Coin))
toCompactCoinNonZero = fmap unsafeNonZero . toCompact . unNonZero

fromCompactCoinNonZero :: NonZero (CompactForm Coin) -> NonZero Coin
fromCompactCoinNonZero = unsafeNonZero . fromCompact . unNonZero

unCoinNonZero :: NonZero Coin -> NonZero Integer
unCoinNonZero = unsafeNonZero . unCoin . unNonZero

toCoinNonZero :: Integral a => NonZero a -> NonZero Coin
toCoinNonZero = unsafeNonZero . Coin . toInteger . unNonZero

compactCoinNonZero :: NonZero Word64 -> NonZero (CompactForm Coin)
compactCoinNonZero = unsafeNonZero . CompactCoin . unNonZero

partialCompactCoinL :: HasCallStack => Lens' (CompactForm Coin) Coin
partialCompactCoinL = lens fromCompact $ const compactCoinOrError
