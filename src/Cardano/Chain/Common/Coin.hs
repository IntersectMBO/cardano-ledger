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

-- This is for 'mkKnownCoin''s @n <= 45000000000000000@ constraint, which is
-- considered redundant. TODO: investigate this.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Chain.Common.Coin
       ( Coin
       , CoinError
       , mkCoin
       , mkKnownCoin
       , coinF

       , maxCoinVal
       , sumCoins

       -- * Conversions
       , unsafeGetCoin
       , coinToInteger
       , integerToCoin

       -- * Arithmetic operations
       , addCoin
       , subCoin
       , scaleCoin
       , divCoin
       ) where

import           Cardano.Prelude

import qualified Data.Aeson as Aeson
    (FromJSON (..), ToJSON (..))
import           Data.Data
    (Data)
import           Formatting
    (Format, bprint, build, int)
import qualified Formatting.Buildable as B
import           GHC.TypeLits
    (type (<=))
import qualified Text.JSON.Canonical as Canonical
    (FromJSON (..), ReportSchemaErrors, ToJSON (..))

import           Cardano.Binary.Class
    (Bi (..))


-- | Coin is the least possible unit of currency
newtype Coin = Coin
  { getCoin :: Word64
  } deriving (Show, Ord, Eq, Generic, Data, NFData)

instance B.Buildable Coin where
  build (Coin n) = bprint (int . " coin(s)") n

instance Bounded Coin where
  minBound = Coin 0
  maxBound = Coin maxCoinVal

instance Bi Coin where
  encode = encode . unsafeGetCoin
  decode = Coin <$> decode
  encodedSizeExpr size pxy = size (unsafeGetCoin <$> pxy)

instance Monad m => Canonical.ToJSON m Coin where
  toJSON = Canonical.toJSON . unsafeGetCoin

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m Coin where
  fromJSON = fmap Coin . Canonical.fromJSON

instance Aeson.FromJSON Coin where
  parseJSON v = do
    c <- Aeson.parseJSON v
    toAesonError $ mkCoin c

instance Aeson.ToJSON Coin where
  toJSON = Aeson.toJSON . unsafeGetCoin

data CoinError
  = CoinOverflow Word64
  | CoinTooLarge Integer
  | CoinTooSmall Integer
  | CoinUnderflow Word64 Word64
  deriving (Eq, Show)

instance B.Buildable CoinError where
  build = \case
    CoinOverflow c -> bprint
      ("Coin value, " . build . ", overflowed")
      c
    CoinTooLarge c -> bprint
      ("Coin value, " . build . ", exceeds maximum, " . build)
      c
      maxCoinVal
    CoinTooSmall c -> bprint
      ("Coin value, " . build . ", is less than minimum, " . build)
      c
      (minBound :: Coin)
    CoinUnderflow c c' -> bprint
      ("Coin underflow when subtracting " . build . " from " . build)
      c'
      c

-- | Maximal possible value of 'Coin'
maxCoinVal :: Word64
maxCoinVal = 45e15

-- | Constructor for 'Coin' returning 'CoinError' when @c@ exceeds 'maxCoinVal'
mkCoin :: Word64 -> Either CoinError Coin
mkCoin c
  | c <= maxCoinVal = Right (Coin c)
  | otherwise       = Left (CoinTooLarge (toInteger c))
{-# INLINE mkCoin #-}

-- | Construct a 'Coin' from a 'KnownNat', known to be less than 'maxCoinVal'
mkKnownCoin :: forall n. (KnownNat n, n <= 45000000000000000) => Coin
mkKnownCoin = Coin . fromIntegral . natVal $ Proxy @n

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

-- | Unwraps 'Coin'. It's called “unsafe” so that people wouldn't use it
-- willy-nilly if they want to sum coins or something. It's actually safe.
unsafeGetCoin :: Coin -> Word64
unsafeGetCoin = getCoin
{-# INLINE unsafeGetCoin #-}

-- | Compute sum of all coins in container. Result is 'Integer' as a protection
--   against possible overflow.
sumCoins :: (Foldable t, Functor t) => t Coin -> Either CoinError Coin
sumCoins = integerToCoin . sum . map coinToInteger

coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unsafeGetCoin
{-# INLINE coinToInteger #-}

-- | Addition of coins, returning 'CoinError' in case of overflow
addCoin :: Coin -> Coin -> Either CoinError Coin
addCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
  | res >= a && res >= b && res <= maxCoinVal = Right (Coin res)
  | otherwise = Left (CoinOverflow res)
  where res = a + b
{-# INLINE addCoin #-}

-- | Subtraction of coins, returning 'CoinError' on underflow
subCoin :: Coin -> Coin -> Either CoinError Coin
subCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
  | a >= b    = Right (Coin (a - b))
  | otherwise = Left (CoinUnderflow a b)

-- | Scale a 'Coin' by an 'Integral' factor, returning 'CoinError' when the
--   result is too large
scaleCoin :: Integral b => Coin -> b -> Either CoinError Coin
scaleCoin (unsafeGetCoin -> a) b
  | res <= coinToInteger (maxBound :: Coin) = Right $ Coin (fromInteger res)
  | otherwise                             = Left $ CoinTooLarge res
  where res = toInteger a * toInteger b
{-# INLINE scaleCoin #-}

-- | Integer division of a 'Coin' by an 'Integral' factor
divCoin :: Integral b => Coin -> b -> Coin
divCoin (unsafeGetCoin -> a) b = Coin (a `div` fromIntegral b)
{-# INLINE divCoin #-}

integerToCoin :: Integer -> Either CoinError Coin
integerToCoin n
  | n < 0     = Left (CoinTooSmall n)
  | n <= coinToInteger (maxBound :: Coin) = Right $ Coin (fromInteger n)
  | otherwise = Left (CoinTooLarge n)
