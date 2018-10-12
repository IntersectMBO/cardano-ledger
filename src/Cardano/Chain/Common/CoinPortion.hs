{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Cardano.Chain.Common.CoinPortion
       ( CoinPortion (..)
       , coinPortionDenominator
       , checkCoinPortion
       , unsafeCoinPortionFromDouble
       , coinPortionToDouble
       , applyCoinPortionDown
       , applyCoinPortionUp
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Formatting (bprint, float, int, sformat, (%))
import qualified Formatting.Buildable as B
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..))

import           Cardano.Binary.Class (Bi (..))
import           Cardano.Chain.Common.Coin


-- | CoinPortion is some portion of Coin; it is interpreted as a fraction
-- with denominator of 'coinPortionDenominator'. The numerator must be in the
-- interval of [0, coinPortionDenominator].
--
-- Usually 'CoinPortion' is used to determine some threshold expressed as
-- portion of total stake.
--
-- To multiply a coin portion by 'Coin', use 'applyCoinPortionDown' (when
-- calculating number of coins) or 'applyCoinPortionUp' (when calculating a
-- threshold).
newtype CoinPortion = CoinPortion
    { getCoinPortion :: Word64
    } deriving (Show, Ord, Eq, Generic, Typeable, NFData)

instance Bi CoinPortion where
    encode = encode . getCoinPortion
    decode = CoinPortion <$> decode

instance Monad m => ToJSON m CoinPortion where
    toJSON = toJSON @_ @Word64 . getCoinPortion  -- i. e. String

instance MonadError SchemaError m => FromJSON m CoinPortion where
    fromJSON val = do
        number <- fromJSON val
        pure $ CoinPortion number

instance Aeson.FromJSON CoinPortion where
    parseJSON v = unsafeCoinPortionFromDouble <$> Aeson.parseJSON v

instance Aeson.ToJSON CoinPortion where
    toJSON = Aeson.toJSON . coinPortionToDouble

-- | Denominator used by 'CoinPortion'.
coinPortionDenominator :: Word64
coinPortionDenominator = (10 :: Word64) ^ (15 :: Word64)

instance Bounded CoinPortion where
    minBound = CoinPortion 0
    maxBound = CoinPortion coinPortionDenominator

-- | Make 'CoinPortion' from 'Word64' checking whether it is not greater
-- than 'coinPortionDenominator'.
checkCoinPortion
    :: MonadError Text m
    => CoinPortion -> m ()
checkCoinPortion (CoinPortion x)
    | x <= coinPortionDenominator = pure ()
    | otherwise = throwError err
  where
    err =
        sformat
            ("CoinPortion: value is greater than coinPortionDenominator: "
            %int) x

-- | Make CoinPortion from Double. Caller must ensure that value is in
-- [0..1]. Internally 'CoinPortion' stores 'Word64' which is divided by
-- 'coinPortionDenominator' to get actual value. So some rounding may take
-- place.
unsafeCoinPortionFromDouble :: Double -> CoinPortion
unsafeCoinPortionFromDouble x
    | 0 <= x && x <= 1 = CoinPortion v
    | otherwise = error "unsafeCoinPortionFromDouble: double not in [0, 1]"
  where
    v = round $ realToFrac coinPortionDenominator * x
{-# INLINE unsafeCoinPortionFromDouble #-}

instance B.Buildable CoinPortion where
    build cp@(getCoinPortion -> x) = bprint
        (int%"/"%int%" (approx. "%float%")")
        x
        coinPortionDenominator
        (coinPortionToDouble cp)

coinPortionToDouble :: CoinPortion -> Double
coinPortionToDouble (getCoinPortion -> x) =
    realToFrac @_ @Double x / realToFrac coinPortionDenominator
{-# INLINE coinPortionToDouble #-}

-- | Apply CoinPortion to Coin (with rounding down).
--
-- Use it for calculating coin amounts.
applyCoinPortionDown :: CoinPortion -> Coin -> Coin
applyCoinPortionDown (getCoinPortion -> p) (unsafeGetCoin -> c) =
    Coin . fromInteger $
        (toInteger p * toInteger c) `div`
        (toInteger coinPortionDenominator)

-- | Apply CoinPortion to Coin (with rounding up).
--
-- Use it for calculating thresholds.
applyCoinPortionUp :: CoinPortion -> Coin -> Coin
applyCoinPortionUp (getCoinPortion -> p) (unsafeGetCoin -> c) =
    let (d, m) = divMod (toInteger p * toInteger c)
                        (toInteger coinPortionDenominator)
    in if m > 0 then Coin (fromInteger (d + 1))
                else Coin (fromInteger d)
