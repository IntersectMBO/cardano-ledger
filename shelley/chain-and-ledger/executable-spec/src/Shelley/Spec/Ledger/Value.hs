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

module Shelley.Spec.Ledger.Value where

import Cardano.Binary (FromCBOR, ToCBOR, encodeListLen, fromCBOR, toCBOR)
import Cardano.Prelude (NFData (), NoUnexpectedThunks (..))
import Data.ByteString (ByteString) -- TODO is this the right Bytestring
import qualified Data.Map as Map
import Data.Map.Internal (Map (..), balanceL, balanceR, link, link2, singleton, splitLookup)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)

-- =============================================================
-- General function and type class definitions used in Value

data Op = Gt | Lt | Gteq | Lteq | Neq | Equal

class
  (NFData t, Show t, Eq t, NoUnexpectedThunks t, Typeable t, FromCBOR t, ToCBOR t) =>
  Val t
  where
  vzero :: t -- This is an identity of vplus
  vplus :: t -> t -> t -- This must be associative and commutative
  vnegate :: t -> t -- vplus x (vnegate x) == vzero
  vscale :: Integer -> t -> t --
  voper :: Op -> t -> t -> Bool

  -- This will define a PARTIAL order using pointwise comparisons
  -- Semantic Equality (i.e. the Eq instance) should be (voper Equal)
  visZero :: t -> Bool -- is the argument vzero?
  vcoin :: t -> Coin -- get the Coin amount
  vinject :: Coin -> t -- inject Coin into the Val instance
  vsize :: t -> Integer -- compute size of Val instance

-- | subtract Val
vminus :: (Val v) => v -> v -> v
vminus v1 v2 = vplus v1 (vnegate v2)

instance Val Integer where
  vzero = 0
  vplus x y = x + y
  vnegate x = - x
  vscale n x = n * x
  voper Gt x y = x > y
  voper Lt x y = x < y
  voper Gteq x y = x >= y
  voper Lteq x y = x <= y
  voper Neq x y = not (x == y)
  voper Equal x y = x == y
  visZero x = x == 0
  vcoin x = Coin x
  vinject (Coin x) = x
  vsize _ = 1

instance
  (Ord k, Val t, NFData k, Show k, NoUnexpectedThunks k, Typeable k, FromCBOR k, ToCBOR k) =>
  Val (Map k t)
  where
  vzero = Map.empty
  vplus x y = unionWithV vplus x y -- There is an assumption that if the range is vzero, it is not stored in the Map
  vnegate x = mapV vnegate x -- We enforce this by using our own versions of map and union: unionWithV and mapV
  vscale n x = mapV (vscale n) x
  voper op x y = pointWise (voper op) x y
  visZero x = Map.null x
  vcoin _ = Coin 0
  vinject _ = Map.empty -- TODO Should not be any Coin in map
  vsize x = fromIntegral $ Map.size x -- TODO shouldnt use this for Value

-- ================================================================
-- Operations on Map, so we cam make Map a Val instance.

-- Pointwise comparison assuming the map is the Default value (vzero) everywhere except where it is defined
pointWise :: (Ord k, Val v) => (v -> v -> Bool) -> Map k v -> Map k v -> Bool
pointWise _ Tip Tip = True
pointWise p Tip (m@(Bin _ _ _ _ _)) = all (vzero `p`) m
pointWise p (m@(Bin _ _ _ _ _)) Tip = all (`p` vzero) m
pointWise p m (Bin _ k v2 ls rs) =
  case Map.splitLookup k m of
    (lm, Just v1, rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
    _ -> False

-- The following functions enforce the invariant that vzero is never stored in a Map
insertWithV :: (Ord k, Val a) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithV = go
  where
    go :: (Ord k, Val a) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = if visZero x then Tip else singleton kx x
    go f !kx x (Bin sy ky y l r) =
      case compare kx ky of
        LT -> balanceL ky y (go f kx x l) r
        GT -> balanceR ky y l (go f kx x r)
        EQ -> if visZero new then link2 l r else Bin sy kx new l r
          where
            new = f x y
{-# INLINEABLE insertWithV #-}

unionWithV :: (Ord k, Val a) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithV _f t1 Tip = t1
unionWithV f t1 (Bin _ k x Tip Tip) = insertWithV f k x t1
unionWithV f (Bin _ k x Tip Tip) t2 = insertWithV f k x t2
unionWithV _f Tip t2 = t2
unionWithV f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
    Nothing -> if visZero x1 then link2 l1l2 r1r2 else link k1 x1 l1l2 r1r2
    Just x2 -> if visZero new then link2 l1l2 r1r2 else link k1 new l1l2 r1r2
      where
        new = (f x1 x2)
    where
      !l1l2 = unionWithV f l1 l2
      !r1r2 = unionWithV f r1 r2
{-# INLINEABLE unionWithV #-}

mapV :: (Ord k, Val a) => (a -> a) -> Map k a -> Map k a
mapV f m = Map.foldrWithKey accum Map.empty m
  where
    accum k v ans = if visZero new then ans else Map.insert k new ans
      where
        new = f v
{-# INLINEABLE mapV #-}

-- ======================================================================
-- Multi Assests
--
-- A Value is a map from 'PolicyID's to a quantity of assets with this policy.
-- This map implements a finitely supported functions ovr PolicyId.
-- A PolicyID is not stored in the Map, then its quantity is assumed to be 0.
--
-- Operations on assets are usually implemented 'pointwise'. That is,
-- we apply the operation to the quantities for each asset in turn. So
-- when we add two 'Value's the resulting 'Value' has, for each asset,
-- the sum of the quantities of /that particular/ asset in the argument
-- 'Value'. The effect of this is that the assets in the 'Value' are "independent",
-- and are operated on separately.
--
-- We can think of 'Value' as a vector space whose dimensions are
-- assets. At the moment there is only a single asset type (Ada), so 'Value'
-- contains one-dimensional vectors. When asset-creating transactions are
-- implemented, this will change and the definition of 'Value' will change to a
-- 'Map Asset Int', effectively a vector with infinitely many dimensions whose
-- non-zero values are recorded in the map.
--
-- To create a value of 'Value', we need to specifiy an asset policy. This can be done
-- using 'Ledger.Ada.adaValueOf'. To get the ada dimension of 'Value' we use
-- 'Ledger.Ada.fromValue'. Plutus contract authors will be able to define modules
-- similar to 'Ledger.Ada' for their own assets.
-- ======================================================================================

-- | Asset ID
newtype AssetID = AssetID {assetID :: ByteString}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData)

-- | Policy ID
newtype PolicyID crypto = PolicyID {policyID :: ScriptHash crypto}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData)

-- | The Value representing MultiAssets
data Value crypto = Value !Integer !(Map (PolicyID crypto) (Map AssetID Integer))
  deriving (Show, Generic)

-- =============================================
-- Operations, and class instances on Value

class Default f t where
  apply :: Ord k => f k t -> k -> t

instance NFData (Value crypto)

deriving instance Val Coin

instance NoUnexpectedThunks (Value crypto)

instance Val t => Default Map t where
  apply mp k = case Map.lookup k mp of Just t -> t; Nothing -> vzero

instance Crypto crypto => Eq (Value crypto) where
  (==) (Value c v) (Value c1 v1) = (voper Equal c c1) && (voper Equal v v1)

instance Crypto crypto => Semigroup (Value crypto) where
  (<>) = vplus

instance Crypto crypto => Monoid (Value crypto) where
  mempty = vzero
  mappend = (<>)

instance Crypto crypto => Val (Value crypto) where
  vzero = Value 0 vzero
  vplus (Value c1 v1) (Value c2 v2) = Value (vplus c1 c2) (vplus v1 v2)
  vnegate (Value c1 v1) = Value (vnegate c1) (vnegate v1)
  vscale s (Value c1 v1) = Value (vscale s c1) (vscale s v1)
  voper op (Value c1 v1) (Value c2 v2) = (voper op c1 c2) && (voper op v1 v2)
  visZero (Value c1 v1) = (visZero c1) && (visZero v1)
  vcoin (Value c1 _) = Coin c1
  vinject (Coin c1) = Value c1 vzero
  vsize (Value _ v) = foldr accum uint v -- add uint for the Coin portion in this size calculation
    where
      accum u ans = foldr accumIns (ans + addrHashLen) u -- add addrHashLen for each Policy ID
        where
          accumIns _ ans1 = ans1 + assetIdLen + uint -- add assetIdLen and uint for each asset of that Policy ID

-- ============================================
-- Constants needed to compute size and size-scaling operation

-- address hash length is always same as Policy ID length
addrHashLen :: Integer
addrHashLen = 28

smallArray :: Integer
smallArray = 1

hashLen :: Integer
hashLen = 32

assetIdLen :: Integer
assetIdLen = 32

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

-- This scaling function is right for UTxO, not EUTxO
scaleVl :: (Val v) => v -> Coin -> Coin
scaleVl v (Coin mv)
  | vinject (vcoin v) == v = Coin mv -- without non-Coin assets, scaled deposit should be exactly minUTxOValue
  | otherwise = Coin $ fst $ quotRem mv (utxoEntrySizeWithoutVal + vsize v) -- round down

-- compare the outputs as Values (finitely supported functions)
-- ada must be greater than scaled min value deposit
-- rest of tokens must be greater than 0
-- outputsTooSmall = [out | out@(TxOut _ vl) <- outputs, (voper Gt) (vinject $ scaleVl vl minUTxOValue) vl]

-- =============================================================
-- Operations needed for Tests

class Val t => ValTest t where
  vsplit :: t -> Integer -> ([t], Coin)
  vmodify :: Monad m => (Coin -> m Coin) -> t -> m t

instance ValTest Coin where
  vsplit (Coin n) 0 = ([], Coin n)
  vsplit (Coin n) m -- TODO fix this?
    | m <= 0 = error "must split coins into positive parts"
    | otherwise = (take (fromIntegral m) (repeat (Coin (n `div` m))), Coin (n `rem` m))
  vmodify f coin = f coin

instance Crypto crypto => ValTest (Value crypto) where
  vsplit (Value coin _) 0 = ([], Coin coin) -- The sum invariant may not hold, but no other way to split into 0 groups
  vsplit (Value coin assets) m = (zipWith Value (map unCoin coins) maps, remainder)
    where
      maps = assets : (take (fromIntegral $ m - 1) (repeat vzero))
      (coins, remainder) = vsplit (Coin coin) m
  vmodify f (Value coin assets) = do (Coin coin2) <- f (Coin coin); pure (Value coin2 assets)

-- ===============================================================
-- constraint used for all parametrized functions

type CV c v = (Val v, Crypto c, Typeable c, ValTest v)

type CVNC c v = (Val v, Typeable c, ValTest v)

-- =====================================================
-- How to add a single new asset to a Value

vinsert :: Crypto crypto => PolicyID crypto -> AssetID -> Integer -> Value crypto -> Value crypto
vinsert pid aid q old = vplus old (Value 0 (Map.singleton pid (Map.singleton aid q)))

-- ====================================================
-- CBOR

instance
  (Crypto crypto) =>
  ToCBOR (Value crypto)
  where
  toCBOR (Value c v) =
    encodeListLen 2
      <> toCBOR c
      <> toCBOR v

instance
  (Crypto crypto) =>
  FromCBOR (Value crypto)
  where
  fromCBOR = do
    decodeRecordNamed "Value" (const 2) $ do
      c <- fromCBOR
      v <- fromCBOR
      pure $ Value c v
