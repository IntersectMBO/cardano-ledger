{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module defines a generalised notion of a "value" - that is, something
-- with which we may quantify a transaction output.
module Cardano.Ledger.Val
  ( Val (..),
    LabeledInt (..),
    scale,
    sumVal,
    scaledMinDeposit,
    addrHashLen,
    uint,
    assetIdLen,
    mapV,
    unionWithV,
    pointWiseM,
    insertWithV,
  )
where

import Data.Map.Internal
  ( Map (..),
    balanceL,
    balanceR,
    link,
    link2,
    singleton,
    splitLookup,
  )
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Coin (Coin (..))

-- ==========================================================================
-- The LabeledInt class describes an algebraic structure. Think of it as big
-- box with a bunch of labelled items. There are two ways to label an item.
-- 1) By path to the item.
-- 2) By associating a key to the item.
-- There is an inductive way to define things in the Val class. We are interested
-- in the case where item is an Integer. So there are three formation rules,
-- which show up as three instances. One for Base the case Integer, one for
-- Binary Paths, and one for Keys realized in a Data.Map.Map
-- All Instances are built composing some form of the 3 rules.

class (Eq t, Show t) => LabeledInt t where
  zeroLI :: t
  plusLI :: t -> t -> t
  minusLI :: t -> t -> t
  minusLI x y = plusLI x (invertLI y)
  scaleLI :: Int -> t -> t
  invertLI :: t -> t
  invertLI x = scaleLI (-1) x
  isZeroLI :: t -> Bool
  pointWiseLI :: (Integer -> Integer -> Bool) -> t -> t -> Bool

-- Base Case
instance LabeledInt Integer where
  zeroLI = 0
  plusLI x y = x + y
  scaleLI n x = fromIntegral n * x
  isZeroLI x = x == 0
  pointWiseLI p x y = p x y

-- Key Case
instance (Typeable k, Show k, Ord k, LabeledInt t) => LabeledInt (Map.Map k t) where
  zeroLI = Map.empty
  plusLI x y = unionWithV plusLI x y
  scaleLI s x = mapV (s `scaleLI`) x
  isZeroLI x = Map.null x
  pointWiseLI p x y = pointWiseM (pointWiseLI p) x y

-- Binary Path Case
instance (LabeledInt x, LabeledInt y) => LabeledInt (x, y) where
  zeroLI = (zeroLI, zeroLI)
  plusLI (x, y) (a, b) = (plusLI x a, plusLI y b)
  scaleLI n (x, y) = (scaleLI n x, scaleLI n y)
  isZeroLI (x, y) = isZeroLI x && isZeroLI y
  pointWiseLI p (x, y) (a, b) = pointWiseLI p x a && pointWiseLI p y b

-- By newtype deriving this instance is basically free.

deriving instance LabeledInt Coin

-- ===================================================================================================
-- THe Val class is a generalization of LabeledInt. It renames the the operations of LabeledInt
-- to give them more concise types, and makes some of them infix operators. The new methods add
-- functionality that relate to the use case in Cardano: to encode Ada coins and Multi-Assets.
-- Having all the methods in 1 class adds simplicity and clarity. The added methods provide two new abilities
-- 1) That Coin is a blessed currency, and that every instance picks one of the embedded integers as
--    the one labeled Coin. And gives means to access and alter this unique item.
-- 2) That we need to compute the size of these things.

infixl 6 <+>

infixl 6 <->

infixl 7 <×>

class (Eq t, Show t, LabeledInt t) => Val t where
  -- | The Val object with a count of 0 for everything
  zero :: t
  zero = zeroLI

  -- | add two Val objects
  (<+>) :: t -> t -> t
  x <+> y = plusLI x y

  -- | Subtract two Val objects
  (<->) :: t -> t -> t
  x <-> y = plusLI x (invertLI y)

  -- | Scale a Val object by a constant
  (<×>) :: Int -> t -> t
  x <×> y = scaleLI x y

  -- | Invert a Val object
  invert :: t -> t
  invert x = scaleLI (-1) x

  -- | Is the argument zero?
  isZero :: t -> Bool
  isZero x = isZeroLI x

  -- | Get the ADA present in the value (since ADA is our "blessed" currency)
  coin :: t -> Coin

  -- | Create a value containing only this amount of ADA
  inject :: Coin -> t

  modifyCoin :: (Coin -> Coin) -> t -> t

  size :: t -> Integer -- compute size of Val instance

instance Val Coin where
  coin x = x
  inject x = x
  modifyCoin f x = f x
  size _x = 1

-- =============================================================
-- Synonym for backward compatibility

scale :: Val t => Int -> t -> t
scale n t = n <×> t

sumVal :: (Foldable t, LabeledInt v) => t v -> v
sumVal xs = foldl plusLI zeroLI xs

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

    smallArray :: Integer
    smallArray = 1

    hashLen :: Integer
    hashLen = 32

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

uint :: Integer
uint = 5

assetIdLen :: Integer
assetIdLen = 32

-- address hash length is always same as Policy ID length
addrHashLen :: Integer
addrHashLen = 2

-- ============================================================================
-- Operations on Map, specialised to comparable `Monoid` values.

-- ======================================================================\
-- We can nest Map.Map over a LabeledInt t, and that inherits from t
-- This means instances like (Map k1 (Map k2 (Map k3 t))) come for free if LabeledInt  t

-- | Pointwise comparison assuming the map is the Default value (zeroLI) everywhere except where it is defined
pointWiseM ::
  (Ord k, LabeledInt v) =>
  (v -> v -> Bool) ->
  Map k v ->
  Map k v ->
  Bool
pointWiseM _ Tip Tip = True
pointWiseM p Tip (m@(Bin _ _ _ _ _)) = all (zeroLI `p`) m
pointWiseM p (m@(Bin _ _ _ _ _)) Tip = all (`p` zeroLI) m
pointWiseM p m (Bin _ k v2 ls rs) =
  case Map.splitLookup k m of
    (lm, Just v1, rm) -> p v1 v2 && pointWiseM p ls lm && pointWiseM p rs rm
    _ -> False

-- | insertWithV enforces the invariant that mempty is never stored in a Map
insertWithV ::
  (Ord k, LabeledInt a) =>
  (a -> a -> a) ->
  k ->
  a ->
  Map k a ->
  Map k a
insertWithV = go
  where
    go ::
      (Ord k, LabeledInt a) =>
      (a -> a -> a) ->
      k ->
      a ->
      Map k a ->
      Map k a
    go _ !kx x Tip = if x == zeroLI then Tip else singleton kx x
    go f !kx x (Bin sy ky y l r) =
      case Prelude.compare kx ky of
        LT -> balanceL ky y (go f kx x l) r
        GT -> balanceR ky y l (go f kx x r)
        EQ -> if new == zeroLI then link2 l r else Bin sy kx new l r
          where
            new = f x y
{-# INLINEABLE insertWithV #-}

unionWithV ::
  (Ord k, LabeledInt a) =>
  (a -> a -> a) ->
  Map k a ->
  Map k a ->
  Map k a
unionWithV _f t1 Tip = t1
unionWithV f t1 (Bin _ k x Tip Tip) = insertWithV f k x t1
unionWithV f (Bin _ k x Tip Tip) t2 = insertWithV f k x t2
unionWithV _f Tip t2 = t2
unionWithV f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
    Nothing ->
      if x1 == zeroLI
        then link2 l1l2 r1r2
        else link k1 x1 l1l2 r1r2
    Just x2 ->
      if new == zeroLI
        then link2 l1l2 r1r2
        else link k1 new l1l2 r1r2
      where
        new = (f x1 x2)
    where
      !l1l2 = unionWithV f l1 l2
      !r1r2 = unionWithV f r1 r2
{-# INLINEABLE unionWithV #-}

mapV :: (Ord k, LabeledInt a) => (a -> a) -> Map k a -> Map k a
mapV f m = Map.foldrWithKey accum Map.empty m
  where
    accum k v ans = if new == zeroLI then ans else Map.insert k new ans
      where
        new = f v
{-# INLINEABLE mapV #-}
