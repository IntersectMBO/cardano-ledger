{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Value
 where

import           Numeric.Natural (Natural)

import           Cardano.Binary (ToCBOR, FromCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Map
import           Cardano.Ledger.Shelley.Crypto
import           Data.Word (Word8)

import           Scripts

-- | Currency ID
-- type CurrencyId crypto = ScriptHash crypto

-- | Token ID
-- type TokenId = String

-- | Quantity
newtype Quantity = Quantity Natural
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Num, Ord, Integral, Real, Enum, NoUnexpectedThunks)

-- instance Num Quanntity where
--    (Quantity q1) + (Quantity q2) = Quantity (q1 + q2)

-- | Value type
data ValueSpec currencyId tokenId = Value (Map currencyId (Map tokenId Quantity))
  deriving (Show, Eq, Generic, Ord, ToCBOR, FromCBOR, NoUnexpectedThunks)

-- | Define Value with the right types
type Value crypto = ValueSpec (ScriptHash crypto) String
--  deriving (Show, Eq, Generic, Ord) -- , ToCBOR, FromCBOR, NoUnexpectedThunks)
--
-- instance NoUnexpectedThunks (Value crypto)
-- deriving instance Crypto crypto => ToCBOR (Value crypto)
-- deriving instance Crypto crypto => FromCBOR (Value crypto)

-- | Overloaded functions for operations on underlying numeric Quantity in
-- | the Value type (in a scalar way)
-- addValue :: (Ord a, Ord b) => Value a b -> Value a b -> Value a b
-- addValue (Value v1) (Value v2) = Value ((unionWith (unionWith (+))) v1 v2)

instance (Ord cidtype, Ord tokentype) => Num (ValueSpec cidtype tokentype) where
   (Value v1) + (Value v2) = Value ((unionWith (unionWith (+))) v1 v2)
   (Value v1) * (Value v2) = Value ((unionWith (unionWith (*))) v1 v2)
   (Value v1) - (Value v2) = Value ((unionWith (unionWith (-))) v1 v2)

-- instance (Ord cidtype, Ord tokentype) => Integral (ValueSpec cidtype tokentype) where
--    floor (ValueSpec v1) =

   -- Pair (a,b) * Pair (c,d) = Pair (a*c,b*d)
   -- Pair (a,b) - Pair (c,d) = Pair (a-c,b-d)
   -- abs    (Pair (a,b)) = Pair (abs a,    abs b)
   -- signum (Pair (a,b)) = Pair (signum a, signum b)
   -- fromInteger i = Pair (fromInteger i, fromInteger i)
