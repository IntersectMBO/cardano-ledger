{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- This is for 'mkKnownLovelace''s @n <= 45000000000000000@ constraint, which is
-- considered redundant. TODO: investigate this.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Chain.Common.Lovelace
  ( Lovelace
  , LovelaceError(..)
  , mkLovelace
  , mkKnownLovelace
  , lovelaceF
  , maxLovelaceVal
  , sumLovelace

       -- * Conversions
  , unsafeGetLovelace
  , lovelaceToInteger
  , integerToLovelace

       -- * Arithmetic operations
  , addLovelace
  , subLovelace
  , scaleLovelace
  , divLovelace
  , modLovelace
  )
where

import Cardano.Prelude

import qualified Data.Aeson as Aeson (FromJSON(..), ToJSON(..))
import Data.Data (Data)
import Formatting (Format, bprint, build, int, sformat)
import qualified Formatting.Buildable as B
import GHC.TypeLits (type (<=))
import qualified Text.JSON.Canonical as Canonical
  (FromJSON(..), ReportSchemaErrors, ToJSON(..))

import Cardano.Binary.Class (Bi(..), DecoderError (..))


-- | Lovelace is the least possible unit of currency
newtype Lovelace = Lovelace
  { getLovelace :: Word64
  } deriving (Show, Ord, Eq, Generic, Data, NFData)

instance B.Buildable Lovelace where
  build (Lovelace n) = bprint (int . " lovelace") n

instance Bounded Lovelace where
  minBound = Lovelace 0
  maxBound = Lovelace maxLovelaceVal

instance Bi Lovelace where
  encode = encode . unsafeGetLovelace
  decode = do
    l <- decode
    toCborError
      . first (DecoderErrorCustom "Lovelace" . sformat build)
      $ mkLovelace l
  encodedSizeExpr size pxy = size (unsafeGetLovelace <$> pxy)

instance Monad m => Canonical.ToJSON m Lovelace where
  toJSON = Canonical.toJSON . unsafeGetLovelace

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m Lovelace where
  fromJSON = fmap Lovelace . Canonical.fromJSON

instance Aeson.FromJSON Lovelace where
  parseJSON v = do
    c <- Aeson.parseJSON v
    toAesonError $ mkLovelace c

instance Aeson.ToJSON Lovelace where
  toJSON = Aeson.toJSON . unsafeGetLovelace

data LovelaceError
  = LovelaceOverflow Word64
  | LovelaceTooLarge Integer
  | LovelaceTooSmall Integer
  | LovelaceUnderflow Word64 Word64
  deriving (Data, Eq, Show)

instance B.Buildable LovelaceError where
  build = \case
    LovelaceOverflow c -> bprint
      ("Lovelace value, " . build . ", overflowed")
      c
    LovelaceTooLarge c -> bprint
      ("Lovelace value, " . build . ", exceeds maximum, " . build)
      c
      maxLovelaceVal
    LovelaceTooSmall c -> bprint
      ("Lovelace value, " . build . ", is less than minimum, " . build)
      c
      (minBound :: Lovelace)
    LovelaceUnderflow c c' -> bprint
      ("Lovelace underflow when subtracting " . build . " from " . build)
      c'
      c

-- | Maximal possible value of 'Lovelace'
maxLovelaceVal :: Word64
maxLovelaceVal = 45e15

-- | Constructor for 'Lovelace' returning 'LovelaceError' when @c@ exceeds
--   'maxLovelaceVal'
mkLovelace :: Word64 -> Either LovelaceError Lovelace
mkLovelace c
  | c <= maxLovelaceVal = Right (Lovelace c)
  | otherwise           = Left (LovelaceTooLarge (toInteger c))
{-# INLINE mkLovelace #-}

-- | Construct a 'Lovelace' from a 'KnownNat', known to be less than
--   'maxLovelaceVal'
mkKnownLovelace :: forall n . (KnownNat n, n <= 45000000000000000) => Lovelace
mkKnownLovelace = Lovelace . fromIntegral . natVal $ Proxy @n

-- | Lovelace formatter which restricts type.
lovelaceF :: Format r (Lovelace -> r)
lovelaceF = build

-- | Unwraps 'Lovelace'. It's called “unsafe” so that people wouldn't use it
--   willy-nilly if they want to sum lovelace or something. It's actually safe.
unsafeGetLovelace :: Lovelace -> Word64
unsafeGetLovelace = getLovelace
{-# INLINE unsafeGetLovelace #-}

-- | Compute sum of all lovelace in container. Result is 'Integer' as a
--   protection against possible overflow.
sumLovelace
  :: (Foldable t, Functor t) => t Lovelace -> Either LovelaceError Lovelace
sumLovelace = integerToLovelace . sum . map lovelaceToInteger

lovelaceToInteger :: Lovelace -> Integer
lovelaceToInteger = toInteger . unsafeGetLovelace
{-# INLINE lovelaceToInteger #-}

-- | Addition of lovelace, returning 'LovelaceError' in case of overflow
addLovelace :: Lovelace -> Lovelace -> Either LovelaceError Lovelace
addLovelace (unsafeGetLovelace -> a) (unsafeGetLovelace -> b)
  | res >= a && res >= b && res <= maxLovelaceVal = Right (Lovelace res)
  | otherwise = Left (LovelaceOverflow res)
  where res = a + b
{-# INLINE addLovelace #-}

-- | Subtraction of lovelace, returning 'LovelaceError' on underflow
subLovelace :: Lovelace -> Lovelace -> Either LovelaceError Lovelace
subLovelace (unsafeGetLovelace -> a) (unsafeGetLovelace -> b)
  | a >= b    = Right (Lovelace (a - b))
  | otherwise = Left (LovelaceUnderflow a b)

-- | Scale a 'Lovelace' by an 'Integral' factor, returning 'LovelaceError' when
--   the result is too large
scaleLovelace :: Integral b => Lovelace -> b -> Either LovelaceError Lovelace
scaleLovelace (unsafeGetLovelace -> a) b
  | res <= lovelaceToInteger (maxBound :: Lovelace) = Right
  $ Lovelace (fromInteger res)
  | otherwise = Left $ LovelaceTooLarge res
  where res = toInteger a * toInteger b
{-# INLINE scaleLovelace #-}

-- | Integer division of a 'Lovelace' by an 'Integral' factor
divLovelace :: Integral b => Lovelace -> b -> Lovelace
divLovelace (unsafeGetLovelace -> a) b = Lovelace (a `div` fromIntegral b)
{-# INLINE divLovelace #-}

-- | Integer modulus of a 'Lovelace' by an 'Integral' factor
modLovelace :: Integral b => Lovelace -> b -> Lovelace
modLovelace (Lovelace a) b = Lovelace (a `mod` fromIntegral b)
{-# INLINE modLovelace #-}

integerToLovelace :: Integer -> Either LovelaceError Lovelace
integerToLovelace n
  | n < 0 = Left (LovelaceTooSmall n)
  | n <= lovelaceToInteger (maxBound :: Lovelace) = Right
  $ Lovelace (fromInteger n)
  | otherwise = Left (LovelaceTooLarge n)
