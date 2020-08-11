{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE StandaloneDeriving  #-}


module Shelley.Spec.Ledger.Value
 where

import           Cardano.Binary (ToCBOR, FromCBOR, toCBOR, fromCBOR, encodeListLen)
import           Cardano.Prelude (NoUnexpectedThunks(..), NFData ())
import           Data.Typeable (Typeable)
import           Shelley.Spec.Ledger.Serialization (decodeRecordNamed)

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Map.Strict(Map)
import qualified Data.Map as Map
import           Data.Map.Internal(Map(..),balanceL,balanceR,singleton,link,splitLookup,link2)

import           Shelley.Spec.Ledger.Crypto
import           Data.ByteString (ByteString) -- TODO is this the right Bytestring
import           Shelley.Spec.Ledger.Scripts

{-
General function and type class definitions used in Value
-}

data Op = Gt | Lt | Gteq | Lteq | Neq | Equal

class (NFData t, Show t, Eq t, NoUnexpectedThunks t) => Val t where
  vzero :: t                          -- This is an identity of vplus
  vplus :: t -> t -> t                 -- This must be associative and commutative
  vnegate:: t -> t                    -- vplus x (vnegate x) == vzero
  scalev:: Integer -> t -> t          --
  -- Equal must be the same as Eq instance
  voper:: Op -> t -> t -> Bool  -- This will define a PARTIAL order using pointwise comparisons (If all the keys don't match returns False)
  visZero:: t -> Bool                 -- is the argument vzero?
  vcoin :: t -> Coin                -- get the Coin amount
  vinject :: Coin -> t                -- inject Coin into the Val instance
  vsize :: t -> Integer               -- compute size of Val instance
  vsplit :: t -> Integer -> (t, Coin)

instance Val Integer where
  vzero = 0
  vplus x y = x+y
  vnegate x = -x
  scalev n x = n * x
  voper Gt x y = x>y
  voper Lt x y = x<y
  voper Gteq x y = x >= y
  voper Lteq x y = x <= y
  voper Neq x y = not(x==y)
  voper Equal x y = x==y
  visZero x = x==0
  vcoin x = Coin x
  vinject (Coin x) = x
  vsize _ = 1
  vsplit n 0 = (0, Coin n)
  vsplit n m
    | m <= 0 = error "must split coins into positive parts"
    | otherwise = (n `div` m, Coin $ n `rem` m)

instance Val t => Default Map t where
   apply mp k = case Map.lookup k mp of { Just t -> t; Nothing -> vzero }

instance (Ord k,Val t, NFData k, Show k, NoUnexpectedThunks k) => Val (Map k t) where
  vzero = Map.empty
  vplus x y = unionWithV vplus x y  -- There is an assumption that if the range is vzero, it is not stored in the Map
  vnegate x = mapV vnegate x        -- We enforce this by using our own versions of map and union: unionWithV and mapV
  scalev n x = mapV (scalev n) x
  voper op x y = pointWise (voper op) x y
  visZero x = Map.null x
  vcoin _ = Coin 0
  vinject _ = Map.empty -- TODO Should not be any Coin in map
  vsize x = fromIntegral $ Map.size x -- TODO shouldnt use this for Value
  vsplit _ _ = (vzero, Coin 0) -- TODO fix

-- Pointwise comparison assuming the map is the Default value everywhere except where it is defined
pointWise:: (Ord k, Val v) => (v -> v -> Bool) -> Map k v -> Map k v -> Bool
pointWise _ Tip Tip = True
pointWise p Tip (m@(Bin _ _ _ _ _)) = all (vzero `p`) m
pointWise p (m@(Bin _ _ _ _ _)) Tip = all ( `p` vzero) m
pointWise p m (Bin _ k v2 ls rs) =
   case Map.splitLookup k m of
      (lm,Just v1,rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
      _ -> False


-- The following functions enforce the invariant that vzero is never stored in a Map
insertWithV :: (Ord k,Val a) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithV = go
  where
    go :: (Ord k,Val a) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = if visZero x then Tip else singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> if visZero new then link2 l r else Bin sy kx new l r
               where new = f x y


{-# INLINABLE insertWithV #-}
unionWithV :: (Ord k,Val a) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithV _f t1 Tip = t1
unionWithV f t1 (Bin _ k x Tip Tip) = insertWithV f k x t1
unionWithV f (Bin _ k x Tip Tip) t2 = insertWithV f k x t2
unionWithV _f Tip t2 = t2
unionWithV f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
      Nothing -> if visZero x1 then link2 l1l2 r1r2 else link k1 x1 l1l2 r1r2
      Just x2 -> if visZero new then link2 l1l2 r1r2 else link k1 new l1l2 r1r2
        where new = (f x1 x2)
    where !l1l2 = unionWithV f l1 l2
          !r1r2 = unionWithV f r1 r2
{-# INLINABLE unionWithV #-}


mapV:: (Ord k,Val a) => (a -> a) -> Map k a -> Map k a
mapV f m = Map.foldrWithKey accum Map.empty m
   where accum k v ans = if visZero new then ans else Map.insert k new ans
            where new = f v
{-# INLINABLE mapV #-}

{-
Value definitions
-}


-- | Quantity
newtype Quantity = Quantity {unInt :: Integer}
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Ord, Integral, Real, Num, Enum, NoUnexpectedThunks, NFData, Val)

-- | Asset ID
newtype AssetID = AssetID {assetID :: ByteString}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData)

-- | Policy ID
newtype PolicyID crypto = PolicyID {policyID :: ScriptHash crypto}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData)

{- note [Assets]

A Value is a map from 'PolicyID's to aquantity of assets with this policy.

Terms of type Value are finitely supported functions. This means :

Operations on assets are usually implemented /pointwise/. That is,
we apply the operation to the quantities for each asset in turn. So
when we add two 'Value's the resulting 'Value' has, for each asset,
the sum of the quantities of /that particular/ asset in the argument
'Value'. The effect of this is that the assets in the 'Value' are "independent",
and are operated on separately.

Whenever we need to get the quantity of an asset in a 'Value' where there
is no explicit quantity of that asset in the 'Value', then the quantity is
taken to be zero.

We can think of 'Value' as a vector space whose dimensions are
assets. At the moment there is only a single asset type (Ada), so 'Value'
contains one-dimensional vectors. When asset-creating transactions are
implemented, this will change and the definition of 'Value' will change to a
'Map Asset Int', effectively a vector with infinitely many dimensions whose
non-zero values are recorded in the map.

To create a value of 'Value', we need to specifiy an asset policy. This can be done
using 'Ledger.Ada.adaValueOf'. To get the ada dimension of 'Value' we use
'Ledger.Ada.fromValue'. Plutus contract authors will be able to define modules
similar to 'Ledger.Ada' for their own assets.

-}

data Value crypto = Value Coin (Map (PolicyID crypto) (Map AssetID Quantity))
  deriving (Show, Generic)

instance NFData (Value crypto)
deriving instance Val Coin
instance NoUnexpectedThunks (Value crypto)

instance Val (Value crypto) where
  vzero = Value (Coin 0) vzero
  vplus (Value c1 v1) (Value c2 v2) = Value (vplus c1 c2) (vplus v1 v2)
  vnegate (Value c1 v1) = Value (vnegate c1) (vnegate v1)
  scalev s (Value c1 v1) = Value (scalev s c1) (scalev s v1)
  voper op (Value c1 v1) (Value c2 v2) = (voper op c1 c2) && (voper op v1 v2)
  visZero (Value c1 v1) = (visZero c1) && (visZero v1)
  vcoin (Value c1 _) = c1
  vinject c1 = Value c1 vzero
  vsize (Value _ v) = foldr accum 1 v where
    accum u ans = foldr accumIns (ans + addrHashLen) u where
      accumIns _ ans1 = ans1 + assetIdLen + uint
  vsplit _ _ = (vzero, Coin 0) -- TODO fix

addrHashLen :: Integer
addrHashLen = 28

assetIdLen :: Integer
assetIdLen = 32

uint :: Integer
uint = 9

class Default f t where
  apply:: Ord k => f k t -> k -> t

instance Eq (Value crypto) where
    (==) (Value c v) (Value c1 v1) = (voper Equal c c1) && (voper Equal v v1)

instance Semigroup (Value crypto) where
    (<>) = vplus

instance Monoid (Value crypto) where
    mempty  = vzero
    mappend = (<>)


-- constraint used for all parametrized functions
type CV c v = (Val v, Crypto c, Typeable c, Typeable v, FromCBOR v, ToCBOR v)
type CVNC c v = (Val v, Typeable c, Typeable v, FromCBOR v, ToCBOR v)

-- Linear Map instance

--
-- -- | Get the quantity of the given currency in the 'Value'.
-- valueOf :: Value crypto -> PolicyID crypto -> AssetID -> Quantity
-- valueOf (Value mp) cur tn =
--     case Data.Map.Strict.lookup cur mp of
--         Nothing -> (Quantity 0)
--         Just i  -> case Data.Map.Strict.lookup tn i of
--             Nothing -> (Quantity 0)
--             Just v  -> v
--
-- -- | The list of 'PolicyID's of a 'Value'.
-- policyIDs :: Value crypto -> [PolicyID crypto]
-- policyIDs (Value mp) = keys mp
--
-- -- | Make a 'Value' containing only the given quantity of the given currency.
-- singleType :: PolicyID crypto -> AssetID -> Quantity -> Value crypto
-- singleType c tn i = Value (singleton c (singleton tn i))


-- Num operations

-- | subtract Val
vminus :: (Val v) => v -> v -> v
vminus v1 v2 = vplus v1 (vnegate v2)
--
-- vinsert:: PolicyID crypto -> AssetID -> Quantity -> Value crypto -> Value crypto
-- vinsert pid aid q old = vplus old (Value (Map.singleton pid (Map.singleton aid q)))

-- | Split a value into its positive and negative parts. The first element of
--   the tuple contains the negative parts of the value, the second element
--   contains the positive parts.
--
--   @negate (fst (split a)) `plus` (snd (split a)) == a@
-- TODO
-- split :: Value crypto -> (Value crypto, Value crypto)
-- split (Value mp) = (negate (Value neg), Value pos) where
--   (neg, pos) = Map.mapThese splitIntl mp
--
--     splitIntl :: Map.Map TokenName Integer -> These (Map.Map TokenName Integer) (Map.Map TokenName Integer)
--     splitIntl mp' = These l r where
--       (l, r) = Map.mapThese (\i -> if i <= 0 then This i else That i) mp'

-- TODO do this right - is this supposed to add up to v?
-- splitValueFee :: Value crypto -> Integer -> (Value crypto, Coin)
-- splitValueFee (Value v) n
--     | n <= 0 = error "must split coins into positive parts"
--     | otherwise = (Value $ fmap (fmap (Quantity . (div n) . unInt)) v, getAdaAmount (Value v))

-- CBOR


instance
  (Crypto crypto)
  => ToCBOR (Value crypto)
 where
   toCBOR (Value c v) =
           encodeListLen 2
           <> toCBOR c
           <> toCBOR v

instance
  (Crypto crypto)
  => FromCBOR (Value crypto)
 where
  fromCBOR = do
    decodeRecordNamed "Value" (const 2) $ do
      c <- fromCBOR
      v <- fromCBOR
      pure $ Value c v
