{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Shelley.Spec.Ledger.Coin
  ( Coin (..),
    word64ToCoin,
    splitCoin,
    coinToRational,
    rationalToCoinViaFloor,
  )
where

import Cardano.Binary (DecoderError (..), FromCBOR (..), ToCBOR (..))
import Cardano.Prelude (NFData, NoUnexpectedThunks (..), cborError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Group (Abelian, Group (..))
import Data.Monoid (Sum (..))
import Data.Text (pack)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Quiet

-- | The amount of value held by a transaction output.
newtype Coin = Coin {unCoin :: Integer}
  deriving
    ( Eq,
      Ord,
      Num,
      Enum,
      NoUnexpectedThunks,
      Generic,
      ToJSON,
      FromJSON,
      NFData
    )
  deriving (Show) via Quiet Coin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer

word64ToCoin :: Word64 -> Coin
word64ToCoin w = Coin $ fromIntegral w

coinToRational :: Coin -> Rational
coinToRational (Coin c) = fromIntegral c

rationalToCoinViaFloor :: Rational -> Coin
rationalToCoinViaFloor r = Coin . floor $ r

isValidCoinValue :: Integer -> Bool
isValidCoinValue c = 0 <= c && c <= (fromIntegral (maxBound :: Word64))

instance ToCBOR Coin where
  toCBOR (Coin c) =
    if isValidCoinValue c
      then toCBOR (fromInteger c :: Word64)
      else toCBOR c

instance FromCBOR Coin where
  fromCBOR = do
    c <- fromCBOR
    if isValidCoinValue c
      then pure (Coin c)
      else cborError $ DecoderErrorCustom "Invalid Coin Value" (pack $ show c)

splitCoin :: Coin -> Integer -> (Coin, Coin)
splitCoin (Coin n) 0 = (Coin 0, Coin n)
splitCoin (Coin n) m
  | m <= 0 = error "must split coins into positive parts"
  | otherwise = (Coin $ n `div` m, Coin $ n `rem` m)
