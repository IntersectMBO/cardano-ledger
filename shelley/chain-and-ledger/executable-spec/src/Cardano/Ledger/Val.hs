{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines a generalised notion of a "value" - that is, something
-- with which we may quantify a transaction output.
--
-- This module is intended for qualified import:
-- > import qualified Cardano.Ledger.Val as Val
module Cardano.Ledger.Val
  ( Val (..),
    (~~),
    scaledMinDeposit,

    -- * Re-exports
    Data.Group.invert,
    (Data.PartialOrd.<=),
    (Data.PartialOrd.>=),
    (Data.PartialOrd.==),
    (Data.PartialOrd./=),
    (Data.PartialOrd.>),
    (Data.PartialOrd.<),
    Data.PartialOrd.compare,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
  )
import Cardano.Prelude (NFData (), NoUnexpectedThunks (..))
import Data.Group (Abelian, Group (invert))
import Data.PartialOrd hiding ((==))
import qualified Data.PartialOrd
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Coin (Coin (..))

class
  ( Abelian t,
    Eq t,
    PartialOrd t,
    -- Do we really need these?
    Show t,
    Typeable t,
    NFData t,
    NoUnexpectedThunks t,
    ToCBOR t,
    FromCBOR t
  ) =>
  Val t
  where
  -- | Multiply the value by a scalar
  scale :: Integral i => i -> t -> t

  -- | Is the argument zero?
  isZero :: t -> Bool
  isZero t = t == mempty

  -- | Get the ADA present in the value (since ADA is our "blessed" currency)
  coin :: t -> Coin

  -- | Create a value containing only this amount of ADA
  inject :: Coin -> t

  size :: t -> Integer -- compute size of Val instance
  -- TODO add PACK/UNPACK stuff to this class

-- | Group subtraction. When we move to groups-0.5 we can export this from
-- there.
(~~) :: Group g => g -> g -> g
a ~~ b = a <> invert b

instance Val Coin where
  scale n (Coin x) = Coin $ (fromIntegral n) * x
  coin = id
  inject = id
  size _ = 1

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
