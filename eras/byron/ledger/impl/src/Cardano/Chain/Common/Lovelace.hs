{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- This is for 'mkKnownLovelace''s @n <= 45000000000000000@ constraint, which is
-- considered redundant. TODO: investigate this.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Chain.Common.Lovelace
  ( -- * Lovelace
    Lovelace,
    LovelaceError (..),
    maxLovelaceVal,

    -- * Constructors
    mkLovelace,
    mkKnownLovelace,

    -- * Formatting
    lovelaceF,

    -- * Conversions
    unsafeGetLovelace,
    lovelaceToInteger,
    integerToLovelace,

    -- * Arithmetic operations
    sumLovelace,
    addLovelace,
    subLovelace,
    scaleLovelace,
    scaleLovelaceRational,
    scaleLovelaceRationalUp,
    divLovelace,
    modLovelace,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    matchSize,
  )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Data.Data (Data)
import Formatting (Format, bprint, build, int, sformat)
import qualified Formatting.Buildable as B
import GHC.TypeLits (type (<=))
import NoThunks.Class (NoThunks (..))
import Quiet
import qualified Text.JSON.Canonical as Canonical
  ( FromJSON (..),
    ReportSchemaErrors,
    ToJSON (..),
  )

-- | Lovelace is the least possible unit of currency
newtype Lovelace = Lovelace
  { unLovelace :: Word64
  }
  deriving (Ord, Eq, Generic, Data, NFData, NoThunks)
  deriving (Show) via (Quiet Lovelace)

instance B.Buildable Lovelace where
  build (Lovelace n) = bprint (int . " lovelace") n

instance Bounded Lovelace where
  minBound = Lovelace 0
  maxBound = Lovelace maxLovelaceVal

-- Used for debugging purposes only
instance ToJSON Lovelace

instance ToCBOR Lovelace where
  toCBOR = toCBOR . unsafeGetLovelace
  encodedSizeExpr size pxy = size (unsafeGetLovelace <$> pxy)

instance FromCBOR Lovelace where
  fromCBOR = do
    l <- fromCBOR
    toCborError
      . first (DecoderErrorCustom "Lovelace" . sformat build)
      $ mkLovelace l

instance Monad m => Canonical.ToJSON m Lovelace where
  toJSON = Canonical.toJSON . unsafeGetLovelace

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m Lovelace where
  fromJSON = fmap Lovelace . Canonical.fromJSON

data LovelaceError
  = LovelaceOverflow Word64
  | LovelaceTooLarge Integer
  | LovelaceTooSmall Integer
  | LovelaceUnderflow Word64 Word64
  deriving (Data, Eq, Show)

instance B.Buildable LovelaceError where
  build = \case
    LovelaceOverflow c ->
      bprint
        ("Lovelace value, " . build . ", overflowed")
        c
    LovelaceTooLarge c ->
      bprint
        ("Lovelace value, " . build . ", exceeds maximum, " . build)
        c
        maxLovelaceVal
    LovelaceTooSmall c ->
      bprint
        ("Lovelace value, " . build . ", is less than minimum, " . build)
        c
        (minBound :: Lovelace)
    LovelaceUnderflow c c' ->
      bprint
        ("Lovelace underflow when subtracting " . build . " from " . build)
        c'
        c

instance ToCBOR LovelaceError where
  toCBOR = \case
    LovelaceOverflow c ->
      encodeListLen 2 <> toCBOR @Word8 0 <> toCBOR c
    LovelaceTooLarge c ->
      encodeListLen 2 <> toCBOR @Word8 1 <> toCBOR c
    LovelaceTooSmall c ->
      encodeListLen 2 <> toCBOR @Word8 2 <> toCBOR c
    LovelaceUnderflow c c' ->
      encodeListLen 3 <> toCBOR @Word8 3 <> toCBOR c <> toCBOR c'

instance FromCBOR LovelaceError where
  fromCBOR = do
    len <- decodeListLen
    let checkSize size = matchSize "LovelaceError" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 2 >> LovelaceOverflow <$> fromCBOR
      1 -> checkSize 2 >> LovelaceTooLarge <$> fromCBOR
      2 -> checkSize 2 >> LovelaceTooSmall <$> fromCBOR
      3 -> checkSize 3 >> LovelaceUnderflow <$> fromCBOR <*> fromCBOR
      _ -> cborError $ DecoderErrorUnknownTag "TxValidationError" tag

-- | Maximal possible value of 'Lovelace'
maxLovelaceVal :: Word64
maxLovelaceVal = 45e15

-- | Constructor for 'Lovelace' returning 'LovelaceError' when @c@ exceeds
--   'maxLovelaceVal'
mkLovelace :: Word64 -> Either LovelaceError Lovelace
mkLovelace c
  | c <= maxLovelaceVal = Right (Lovelace c)
  | otherwise = Left (LovelaceTooLarge (toInteger c))
{-# INLINE mkLovelace #-}

-- | Construct a 'Lovelace' from a 'KnownNat', known to be less than
--   'maxLovelaceVal'
mkKnownLovelace :: forall n. (KnownNat n, n <= 45000000000000000) => Lovelace
mkKnownLovelace = Lovelace . fromIntegral . natVal $ Proxy @n

-- | Lovelace formatter which restricts type.
lovelaceF :: Format r (Lovelace -> r)
lovelaceF = build

-- | Unwraps 'Lovelace'. It's called “unsafe” so that people wouldn't use it
--   willy-nilly if they want to sum lovelace or something. It's actually safe.
unsafeGetLovelace :: Lovelace -> Word64
unsafeGetLovelace = unLovelace
{-# INLINE unsafeGetLovelace #-}

-- | Compute sum of all lovelace in container. Result is 'Integer' as a
--   protection against possible overflow.
sumLovelace ::
  (Foldable t, Functor t) => t Lovelace -> Either LovelaceError Lovelace
sumLovelace = integerToLovelace . sum . map lovelaceToInteger

lovelaceToInteger :: Lovelace -> Integer
lovelaceToInteger = toInteger . unsafeGetLovelace
{-# INLINE lovelaceToInteger #-}

-- | Addition of lovelace, returning 'LovelaceError' in case of overflow
addLovelace :: Lovelace -> Lovelace -> Either LovelaceError Lovelace
addLovelace (Lovelace a) (Lovelace b)
  | res >= a && res >= b && res <= maxLovelaceVal = Right (Lovelace res)
  | otherwise = Left (LovelaceOverflow res)
  where
    res = a + b
{-# INLINE addLovelace #-}

-- | Subtraction of lovelace, returning 'LovelaceError' on underflow
subLovelace :: Lovelace -> Lovelace -> Either LovelaceError Lovelace
subLovelace (Lovelace a) (Lovelace b)
  | a >= b = Right (Lovelace (a - b))
  | otherwise = Left (LovelaceUnderflow a b)

-- | Scale a 'Lovelace' by an 'Integral' factor, returning 'LovelaceError' when
--   the result is too large
scaleLovelace :: Integral b => Lovelace -> b -> Either LovelaceError Lovelace
scaleLovelace (Lovelace a) b = integerToLovelace $ toInteger a * toInteger b
{-# INLINE scaleLovelace #-}

-- | Scale a 'Lovelace' by a rational factor, rounding down.
scaleLovelaceRational :: Lovelace -> Rational -> Lovelace
scaleLovelaceRational (Lovelace a) b =
  Lovelace $ fromInteger $ toInteger a * n `div` d
  where
    n, d :: Integer
    n = numerator b
    d = denominator b

-- | Scale a 'Lovelace' by a rational factor, rounding up.
scaleLovelaceRationalUp :: Lovelace -> Rational -> Lovelace
scaleLovelaceRationalUp (Lovelace a) b =
  Lovelace $ fromInteger $ ceiling $ toRational a * b

-- | Integer division of a 'Lovelace' by an 'Integral' factor
divLovelace :: Integral b => Lovelace -> b -> Either LovelaceError Lovelace
divLovelace (Lovelace a) b = integerToLovelace $ toInteger a `div` toInteger b
{-# INLINE divLovelace #-}

-- | Integer modulus of a 'Lovelace' by an 'Integral' factor
modLovelace :: Integral b => Lovelace -> b -> Either LovelaceError Lovelace
modLovelace (Lovelace a) b = integerToLovelace $ toInteger a `mod` toInteger b
{-# INLINE modLovelace #-}

integerToLovelace :: Integer -> Either LovelaceError Lovelace
integerToLovelace n
  | n < 0 = Left (LovelaceTooSmall n)
  | n <= lovelaceToInteger (maxBound :: Lovelace) =
      Right $
        Lovelace (fromInteger n)
  | otherwise = Left (LovelaceTooLarge n)
