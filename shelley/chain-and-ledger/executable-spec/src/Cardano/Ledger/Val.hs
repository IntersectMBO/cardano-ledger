{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module defines a generalised notion of a "value" - that is, something
-- with which we may quantify a transaction output.
--
module Cardano.Ledger.Val
  ( Val (..),
    scale,
    sumVal,
    scaledMinDeposit,
    addrHashLen,
    uint,
    assetIdLen,
    mapV,
    unionWithV,
    pointWise,
    insertWithV
  )
where

import Data.Typeable (Typeable)
import qualified Data.Map.Strict as Map
import Data.Map.Internal
  ( Map (..),
    balanceL,
    balanceR,
    link,
    link2,
    singleton,
    splitLookup,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))

-- ============================================================

infixl 6 <+>
infixl 6 <->
infixl 7 <×>

class ( Eq t, Show t ) => Val t
  where
     -- | The Val object with a count of 0 for everything
     zero :: t

     -- | add two Val objects
     (<+>) :: t -> t -> t

     -- | Subtract two Val objects
     (<->) :: t -> t -> t
     x <-> y = x <+> (invert y)

     -- | Scale a Val object by a constant
     (<×>) :: Int -> t -> t

     -- | Invert a Val object
     invert :: t -> t
     invert x = (-1) <×> x

     -- | Is the argument zero?
     isZero :: t -> Bool

     -- | Get the ADA present in the value (since ADA is our "blessed" currency)
     coin :: t -> Coin

     -- | Create a value containing only this amount of ADA
     inject :: Coin -> t

     size :: t -> Integer -- compute size of Val instance

-- =============================================================
-- Synonym for backward compatibility

scale :: Val t => Int -> t -> t
scale n t = n <×> t

sumVal :: (Foldable t, Val v) => t v -> v
sumVal xs = foldl (<+>) zero xs


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


addrHashLen :: Integer
addrHashLen = 28

uint :: Integer
uint = 5

assetIdLen :: Integer
assetIdLen = 30   -- I have no idea what this is supposed to be

-- ================================================================================================
-- There are 2 basic instances of the Val class, and most other instances are built from them
-- from some kind of automatic inheritance from these two types.

instance Val Integer where
  zero = 0
  x <+> y = x + y
  n <×> x = fromIntegral n * x
  coin x = Coin x
  inject (Coin x) = x
  size _ = 1
  isZero x = x==0

-- ==================================================================
-- The Coin instance inherits from Integer by newtype deriving

deriving instance Val Coin

-- ======================================================================\
-- We can nest Map.Map over a Val t, and that inherits from t
-- This means instances like (Map k1 (Map k2 (Map k3 t))) come for free if Val t

instance (Typeable k,Show k,Ord k,Val t) => Val (Map.Map k t) where
  zero = Map.empty
  x <+> y = unionWithV (<+>) x y
  s <×> x = mapV (s <×>) x
  isZero x = Map.null x
  coin _ = Coin 0
  inject (Coin _) = Map.empty
  size _ = 1


-- | Pointwise comparison assuming the map is the Default value (zero) everywhere except where it is defined
pointWise ::
  (Ord k, Val v) =>
  (v -> v -> Bool) ->
  Map k v ->
  Map k v ->
  Bool
pointWise _ Tip Tip = True
pointWise p Tip (m@(Bin _ _ _ _ _)) = all (zero `p`) m
pointWise p (m@(Bin _ _ _ _ _)) Tip = all (`p` zero) m
pointWise p m (Bin _ k v2 ls rs) =
  case Map.splitLookup k m of
    (lm, Just v1, rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
    _ -> False

-- | insertWithV enforces the invariant that mempty is never stored in a Map
insertWithV ::
  (Ord k, Val a) =>
  (a -> a -> a) ->
  k ->
  a ->
  Map k a ->
  Map k a
insertWithV = go
  where
    go ::
      (Ord k, Val a) =>
      (a -> a -> a) ->
      k ->
      a ->
      Map k a ->
      Map k a
    go _ !kx x Tip = if x == zero then Tip else singleton kx x
    go f !kx x (Bin sy ky y l r) =
      case Prelude.compare kx ky of
        LT -> balanceL ky y (go f kx x l) r
        GT -> balanceR ky y l (go f kx x r)
        EQ -> if new == zero then link2 l r else Bin sy kx new l r
          where
            new = f x y
{-# INLINEABLE insertWithV #-}

unionWithV ::
  (Ord k, Val a) =>
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
      if x1 == zero
        then link2 l1l2 r1r2
        else link k1 x1 l1l2 r1r2
    Just x2 ->
      if new == zero
        then link2 l1l2 r1r2
        else link k1 new l1l2 r1r2
      where
        new = (f x1 x2)
    where
      !l1l2 = unionWithV f l1 l2
      !r1r2 = unionWithV f r1 r2
{-# INLINEABLE unionWithV #-}

mapV :: (Ord k, Val a) => (a -> a) -> Map k a -> Map k a
mapV f m = Map.foldrWithKey accum Map.empty m
  where
    accum k v ans = if new == zero then ans else Map.insert k new ans
      where
        new = f v
{-# INLINEABLE mapV #-}
