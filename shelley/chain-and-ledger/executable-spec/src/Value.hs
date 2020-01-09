{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Value
 where

import           Numeric.Natural (Natural)

import           Cardano.Binary (ToCBOR, FromCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Cardano.Ledger.Shelley.Crypto
--import           Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Map.Strict (Map, unionWith, empty)
import           Data.Word (Word8)
import           Cardano.Binary (Decoder, FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeBreakOr,
                     decodeListLen, decodeListLenOrIndef, decodeMapLenOrIndef, decodeWord,
                     encodeBreak, encodeListLen, encodeListLenIndef, encodeMapLen, encodeWord,
                     enforceSize, matchSize)
--import           BaseTypes (CborSeq (..), UnitInterval, invalidKey)
--import           Serialization (CBORGroup (..), FromCBORGroup (..), ToCBORGroup (..))
--import           Cardano.Ledger.Shelley.Crypto
--import           Data.Word (Word8)

--import           Scripts


-- | Quantity
newtype Quantity = Quantity Natural
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Num, Ord, Real, Integral, Enum, NoUnexpectedThunks)


-- | Value type
data Value crypto = Value (Map (ScriptHash crypto) (Map String Quantity))
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (Value crypto)

-- | Overloaded functions for operations on underlying numeric Quantity in
-- | the Value type (in a scalar way)

instance Num (Value crypto) where
   (Value v1) + (Value v2) = Value ((unionWith (unionWith (+))) v1 v2)
   (Value v1) * (Value v2) = Value ((unionWith (unionWith (*))) v1 v2)
   (Value v1) - (Value v2) = Value ((unionWith (unionWith (-))) v1 v2)

-- | CBOR temp


instance
  (Crypto crypto)
  => ToCBOR (Value crypto)
  where
    toCBOR v = toCBOR (1 :: Word8)

instance
  Crypto crypto
  => FromCBOR (Value crypto)
 where
  fromCBOR =
      pure $ (Value empty)
