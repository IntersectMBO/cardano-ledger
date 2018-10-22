{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Cardano.Chain.Common.CoinPortion
       ( CoinPortion (..)
       , mkCoinPortion
       , coinPortionDenominator
       , coinPortionFromDouble
       , coinPortionToDouble
       , applyCoinPortionDown
       , applyCoinPortionUp
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (..))
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Formatting (bprint, build, float, int, sformat)
import qualified Formatting.Buildable as B
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..))

import           Cardano.Binary.Class (Bi (..))
import           Cardano.Chain.Common.Coin


-- | CoinPortion is some portion of Coin; it is interpreted as a fraction with
--   denominator of 'coinPortionDenominator'. The numerator must be in the
--   interval of [0, coinPortionDenominator].
--
--   Usually 'CoinPortion' is used to determine some threshold expressed as
--   portion of total stake.
--
--   To multiply a coin portion by 'Coin', use 'applyCoinPortionDown' (when
--   calculating number of coins) or 'applyCoinPortionUp' (when calculating a
--   threshold).
newtype CoinPortion = CoinPortion
  { getCoinPortion :: Word64
  } deriving (Show, Ord, Eq, Generic, Typeable, NFData)

instance B.Buildable CoinPortion where
  build cp@(getCoinPortion -> x) = bprint
    (int . "/" . int . " (approx. " . float . ")")
    x
    coinPortionDenominator
    (coinPortionToDouble cp)

instance Bi CoinPortion where
  encode = encode . getCoinPortion
  decode = CoinPortion <$> decode

instance Monad m => ToJSON m CoinPortion where
  toJSON = toJSON . getCoinPortion

instance MonadError SchemaError m => FromJSON m CoinPortion where
  fromJSON val = do
    number <- fromJSON val
    pure $ CoinPortion number

instance Aeson.FromJSON CoinPortion where
  parseJSON v = do
    c <- Aeson.parseJSON v
    toAesonError $ coinPortionFromDouble c

instance Aeson.ToJSON CoinPortion where
  toJSON = Aeson.toJSON . coinPortionToDouble

-- | Denominator used by 'CoinPortion'.
coinPortionDenominator :: Word64
coinPortionDenominator = 1e15

instance Bounded CoinPortion where
  minBound = CoinPortion 0
  maxBound = CoinPortion coinPortionDenominator

data CoinPortionError
  = CoinPortionDoubleOutOfRange Double
  | CoinPortionTooLarge Word64

instance B.Buildable CoinPortionError where
  build = \case
    CoinPortionDoubleOutOfRange d -> bprint
      ( "Double, "
      . build
      . " , out of range [0, 1] when constructing CoinPortion"
      )
      d
    CoinPortionTooLarge c -> bprint
      ("CoinPortion, " . build . ", exceeds maximum, " . build)
      c
      coinPortionDenominator

-- | Constructor for 'CoinPortion', returning 'CoinPortionError' when @c@
--   exceeds 'coinPortionDenominator'
mkCoinPortion :: Word64 -> Either CoinPortionError CoinPortion
mkCoinPortion c
  | c <= coinPortionDenominator = Right (CoinPortion c)
  | otherwise                   = Left (CoinPortionTooLarge c)

-- | Make CoinPortion from Double. Caller must ensure that value is in [0..1].
--   Internally 'CoinPortion' stores 'Word64' which is divided by
--   'coinPortionDenominator' to get actual value. So some rounding may take
--   place.
coinPortionFromDouble :: Double -> Either CoinPortionError CoinPortion
coinPortionFromDouble x
  | 0 <= x && x <= 1 = Right (CoinPortion v)
  | otherwise        = Left (CoinPortionDoubleOutOfRange x)
  where v = round $ realToFrac coinPortionDenominator * x
{-# INLINE coinPortionFromDouble #-}

coinPortionToDouble :: CoinPortion -> Double
coinPortionToDouble (getCoinPortion -> x) =
  realToFrac x / realToFrac coinPortionDenominator
{-# INLINE coinPortionToDouble #-}

-- | Apply CoinPortion to Coin (with rounding down)
--
--   Use it for calculating coin amounts.
applyCoinPortionDown :: CoinPortion -> Coin -> Coin
applyCoinPortionDown (getCoinPortion -> p) (unsafeGetCoin -> c) = case c' of
  Right coin -> coin
  Left  err  -> panic $ sformat
    ("The impossible happened in applyCoinPortionDown: " . build)
    err
 where
  c' =
    mkCoin
      .     fromInteger
      $     toInteger p
      *     toInteger c
      `div` toInteger coinPortionDenominator

-- | Apply CoinPortion to Coin (with rounding up)
--
--   Use it for calculating thresholds.
applyCoinPortionUp :: CoinPortion -> Coin -> Coin
applyCoinPortionUp (getCoinPortion -> p) (unsafeGetCoin -> c) =
  case mkCoin c' of
    Right coin -> coin
    Left  err  -> panic $ sformat
      ("The impossible happened in applyCoinPortionUp: " . build)
      err
 where
  (d, m) =
    divMod (toInteger p * toInteger c) (toInteger coinPortionDenominator)
  c' = if m > 0 then fromInteger (d + 1) else fromInteger d
