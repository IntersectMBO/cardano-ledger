{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE RoleAnnotations #-}

module Cardano.Ledger.BaseTypes.NonZero (
  KnownBounds (..),
  HasZero (..),
  WithinBounds,
  NonZero,
  unNonZero,
  nonZero,
  knownNonZero,
  knownNonZeroBounded,
  (%.),
  bindNonZero,
  mapNonZero,
  unsafeNonZero,
  toIntegerNonZero,
  (/.),
  nonZeroOr,
  recipNonZero,
  negateNonZero,
  mulNonZero,
  mulNonZeroNat,
  toRatioNonZero,
  (%?),
) where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR, FromCBOR (..), ToCBOR (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio, numerator, (%))
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits
import NoThunks.Class (NoThunks)

class KnownBounds a where
  type MinBound a :: Nat
  type MaxBound a :: Nat

instance KnownBounds Word8 where
  type MinBound Word8 = 0
  type MaxBound Word8 = 0xFF

instance KnownBounds Word16 where
  type MinBound Word16 = 0
  type MaxBound Word16 = 0xFFFF

instance KnownBounds Word32 where
  type MinBound Word32 = 0
  type MaxBound Word32 = 0xFFFFFFFF

instance KnownBounds Word64 where
  type MinBound Word64 = 0
  type MaxBound Word64 = 0xFFFFFFFFFFFFFFFF

type WithinBounds n a = (MinBound a <= n, n <= MaxBound a)

newtype NonZero a = NonZero {unNonZero :: a}
  deriving (Eq, Ord, Show, NoThunks, NFData)
  deriving newtype (EncCBOR, ToCBOR, ToJSON)

type role NonZero nominal

class HasZero a where
  isZero :: a -> Bool
  default isZero :: (Eq a, Num a) => a -> Bool
  isZero = (== 0)

instance HasZero Word8

instance HasZero Word16

instance HasZero Word32

instance HasZero Word64

instance HasZero Integer

instance HasZero Int

instance HasZero Natural

instance HasZero a => HasZero (Ratio a) where
  isZero = isZero . numerator

instance (Typeable a, DecCBOR a, HasZero a) => DecCBOR (NonZero a) where
  decCBOR = decCBOR >>= nonZeroM

instance (HasZero a, FromCBOR a) => FromCBOR (NonZero a) where
  fromCBOR = nonZeroM =<< fromCBOR

instance (FromJSON a, HasZero a) => FromJSON (NonZero a) where
  parseJSON v = parseJSON v >>= nonZeroM

knownNonZero ::
  forall (n :: Nat).
  (KnownNat n, 1 <= n) =>
  NonZero Integer
knownNonZero = NonZero (natVal $ Proxy @n)

knownNonZeroBounded ::
  forall (n :: Nat) a.
  (KnownNat n, 1 <= n, WithinBounds n a, Num a) =>
  NonZero a
knownNonZeroBounded = NonZero (fromInteger . natVal $ Proxy @n)

nonZero :: HasZero a => a -> Maybe (NonZero a)
nonZero = nonZeroM

nonZeroM :: (HasZero a, MonadFail m) => a -> m (NonZero a)
nonZeroM x
  | isZero x = fail "Encountered zero while trying to construct a NonZero value"
  | otherwise = pure $ NonZero x

nonZeroOr :: HasZero a => a -> NonZero a -> NonZero a
nonZeroOr x d = fromMaybe d $ nonZero x

mapNonZero :: (Eq b, HasZero b) => (a -> b) -> NonZero a -> Maybe (NonZero b)
mapNonZero f (NonZero x) = nonZero $ f x

bindNonZero :: (a -> NonZero b) -> NonZero a -> NonZero b
bindNonZero f (NonZero x) = f x

unsafeNonZero :: a -> NonZero a
unsafeNonZero = NonZero

infixl 7 %.

(%.) :: Integral a => a -> NonZero a -> Ratio a
x %. y = x % unNonZero y

infixl 7 %?

(%?) :: Integral a => a -> a -> Ratio a
x %? y
  | y == 0 = 0
  | otherwise = x % y

toIntegerNonZero :: Integral a => NonZero a -> NonZero Integer
toIntegerNonZero (NonZero x) = NonZero $ toInteger x

infixl 7 /.

(/.) :: Fractional a => a -> NonZero a -> a
x /. y = x / unNonZero y

-- Common safe functions

toRatioNonZero :: Integral a => NonZero a -> NonZero (Ratio a)
toRatioNonZero (NonZero x) = NonZero $ x % 1

recipNonZero :: Integral a => NonZero (Ratio a) -> NonZero (Ratio a)
recipNonZero (NonZero x) = NonZero $ recip x

negateNonZero :: NonZero Integer -> NonZero Integer
negateNonZero (NonZero x) = NonZero $ negate x

mulNonZero :: (Integral a, Integral b) => NonZero a -> NonZero b -> NonZero Integer
mulNonZero (NonZero x) (NonZero y) = NonZero $ toInteger x * toInteger y

mulNonZeroNat :: forall n a. (KnownNat n, 1 <= n, Integral a) => NonZero a -> NonZero Integer
mulNonZeroNat (NonZero x) = NonZero (toInteger x * natVal (Proxy @n))
