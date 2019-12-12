{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coin
    (
     Coin(..)
    , splitCoin
    ) where

import           Cardano.Binary (FromCBOR, ToCBOR (toCBOR))
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.Word (Word64)

-- |The amount of value held by a transaction output.
newtype Coin = Coin Integer
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum, NoUnexpectedThunks, FromCBOR)

instance ToCBOR Coin where
  toCBOR (Coin c) = toCBOR (fromInteger c :: Word64)

splitCoin :: Coin -> Integer -> (Coin, Coin)
splitCoin (Coin n) 0 = (Coin 0, Coin n)
splitCoin (Coin n) m
 | m < 0     = error "cannot split into negative parts"
 | otherwise = (Coin $ n `div` m, Coin $ n `rem` m)
