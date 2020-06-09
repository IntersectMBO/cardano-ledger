{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Shelley.Spec.Ledger.Coin
  ( Coin (..),
    splitCoin,
  )
where

import Cardano.Binary (DecoderError (..), FromCBOR (..), ToCBOR (..))
import Cardano.Prelude (NoUnexpectedThunks (..), cborError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (pack)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | The amount of value held by a transaction output.
newtype Coin = Coin Integer
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum, NoUnexpectedThunks, Generic, ToJSON, FromJSON)

instance ToCBOR Coin where
  toCBOR (Coin c) =
    if c >= 0
      then toCBOR (fromInteger c :: Word64)
      else toCBOR c

instance FromCBOR Coin where
  fromCBOR = do
    c <- fromCBOR
    if c >= 0
      then pure (Coin c)
      else cborError $ DecoderErrorCustom "Negative Coin" (pack $ show c)

splitCoin :: Coin -> Integer -> (Coin, Coin)
splitCoin (Coin n) 0 = (Coin 0, Coin n)
splitCoin (Coin n) m
  | m <= 0 = error "must split coins into positive parts"
  | otherwise = (Coin $ n `div` m, Coin $ n `rem` m)
