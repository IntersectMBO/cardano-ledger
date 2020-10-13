{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shelley.Spec.Ledger.Coin
  ( Coin,
    ASSET (Coin),
    Core.CompactForm (..),
    DeltaCoin (..),
    word64ToCoin,
    coinToRational,
    rationalToCoinViaFloor,
    addDelta,
    toDelta,
    scaledMinDeposit,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Val (ASSET, Asset (Ada), Val (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Group (Abelian, Group (..))
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

-- | The amount of value held by a transaction output.
newtype instance ASSET 'Ada () = Coin {unCoin :: Integer}
  deriving
    ( Eq,
      Ord,
      Enum,
      NoThunks,
      Generic,
      ToJSON,
      FromJSON,
      NFData
    )
  deriving (Show) via Quiet Coin
  deriving (ToCBOR, FromCBOR) via Core.Compact Coin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd)

type Coin = ASSET 'Ada ()

instance Val Coin where
  n <×> (Coin x) = Coin $ (fromIntegral n) * x
  coin = id
  inject = id
  size _ = 1
  modifyCoin f v = f v
  pointwise p (Coin x) (Coin y) = p x y

newtype DeltaCoin = DeltaCoin Integer
  deriving (Eq, Ord, Generic, Enum, NoThunks, NFData, FromCBOR, ToCBOR)
  deriving (Show) via Quiet DeltaCoin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd)

addDelta :: Coin -> DeltaCoin -> Coin
addDelta (Coin x) (DeltaCoin y) = Coin (x + y)

toDelta :: Coin -> DeltaCoin
toDelta (Coin x) = DeltaCoin x

word64ToCoin :: Word64 -> Coin
word64ToCoin w = Coin $ fromIntegral w

coinToRational :: Coin -> Rational
coinToRational (Coin c) = fromIntegral c

rationalToCoinViaFloor :: Rational -> Coin
rationalToCoinViaFloor r = Coin . floor $ r

-- FIXME:
-- if coin is less than 0 or greater than (maxBound :: Word64), then
-- fromIntegral constructs the incorrect value. for now this is handled
-- with an erroring bounds check here. where should this really live?
instance Core.Compactible Coin where
  newtype CompactForm Coin = CompactCoin Word64
  toCompact (Coin c)
    | c < 0 = error $ "out of bounds : " ++ show c
    | c > (fromIntegral (maxBound :: Word64)) =
      error $ "out of bounds : " ++ show c
    | otherwise = CompactCoin (fromIntegral c)
  fromCompact (CompactCoin c) = word64ToCoin c

instance ToCBOR (Core.CompactForm Coin) where
  toCBOR (CompactCoin c) = toCBOR c

instance FromCBOR (Core.CompactForm Coin) where
  fromCBOR = CompactCoin <$> fromCBOR

-- ==========================================================
{- The scaledMinDeposit calculation uses the minUTxOValue protocol parameter
(passed to it as Coin mv) as a specification of "the cost of
making a Shelley-sized UTxO entry", calculated here by "utxoEntrySizeWithoutVal + uint",
using the constants in the "where" clause.

In the case when a UTxO entry contains coins only (and the Shelley
UTxO entry format is used - we will extend this to be correct for other
UTxO formats shortly), the deposit should be exactly the minUTxOValue.
This is the "inject (coin v) == v" case.

Otherwise, we calculate the per-byte deposit by multiplying the minimum deposit (which is
for the number of Shelley UTxO-entry bytes) by the size of a Shelley UTxO entry.
This is the "(mv * (utxoEntrySizeWithoutVal + uint))" calculation.

We then calculate the total deposit required for making a UTxO entry with a Val-class
member v by dividing "(mv * (utxoEntrySizeWithoutVal + uint))" by the
estimated total size of the UTxO entry containing v, ie by
"(utxoEntrySizeWithoutVal + size v)".

See the formal specification for details.
-}

-- TODO : This scaling function is right for UTxO, not EUTxO
-- constants are temporary, the UTxO entry size calculation will be moved
scaledMinDeposit :: (Val v) => v -> Coin -> Coin
scaledMinDeposit v (Coin mv)
  | inject (coin v) == v = Coin mv -- without non-Coin assets, scaled deposit should be exactly minUTxOValue
  | otherwise = Coin $ fst $ quotRem (mv * (utxoEntrySizeWithoutVal + uint)) (utxoEntrySizeWithoutVal + size v) -- round down
  where
    -- address hash length is always same as Policy ID length
    addrHashLen :: Integer
    addrHashLen = 28

    smallArray :: Integer
    smallArray = 1

    hashLen :: Integer
    hashLen = 32

    uint :: Integer
    uint = 5

    hashObj :: Integer
    hashObj = 2 + hashLen

    addrHeader :: Integer
    addrHeader = 1

    address :: Integer
    address = 2 + addrHeader + 2 * addrHashLen

    -- input size
    inputSize :: Integer
    inputSize = smallArray + uint + hashObj

    -- size of output not including the Val (compute that part with vsize later)
    outputSizeWithoutVal :: Integer
    outputSizeWithoutVal = smallArray + address

    -- size of the UTxO entry (ie the space the scaled minUTxOValue deposit pays)
    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = inputSize + outputSizeWithoutVal
