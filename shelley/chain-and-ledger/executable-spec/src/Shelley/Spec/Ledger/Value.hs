{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ConstraintKinds            #-}


module Shelley.Spec.Ledger.Value
 where

import           Cardano.Binary (ToCBOR, FromCBOR, toCBOR, fromCBOR, encodeListLen,
                  decodeWord)
import           Cardano.Prelude (NoUnexpectedThunks(..), NFData ())
import           Data.Coerce (coerce)
import           Data.Typeable (Typeable)

import           Shelley.Spec.Ledger.BaseTypes (invalidKey)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Word (Word8)
import           Cardano.Crypto.Hash (Hash, ShortHash)
import           Data.Map.Strict(Map, lookup, toList, elems, filterWithKey, keys)
import qualified Data.Map as Map
import           Data.Map.Internal(Map(..),balanceL,balanceR,singleton,link,splitLookup,link2)

import           Shelley.Spec.Ledger.Crypto
import           Data.ByteString (ByteString) -- TODO is this the right Bytestring
import           Shelley.Spec.Ledger.Scripts

{-
General function and type class definitions used in Value
-}

data Op = Gt | Lt | Gteq | Lteq | Neq | Equal

class (Typeable t, NFData t, Show t, Eq t, NoUnexpectedThunks t, ToCBOR t, FromCBOR t, ProjectToBase t b) => Val t where
  zeroV :: t                          -- This is an identity of addv
  adaTag :: c                         -- tag to look up the Ada part
  addv :: t -> t -> t                 -- This must be associative and commutative
  vnegate:: t -> t                    -- addv x (vnegate x) == zeroV
  scalev:: Integer -> t -> t          --
  -- Equal must be the same as Eq instance
  checkBinRel:: Op -> t -> t -> Bool  -- This will define a PARTIAL order using pointwise comparisons (If all the keys don't match returns False)
  visZero:: t -> Bool                 -- is the argument zeroV?

class Default f t where
  apply:: Ord k => f k t -> k -> t

class ProjectToBase t b where
  projectB :: t -> b

instance Val Integer where
  zeroV = 0
  addv x y = x+y
  vnegate x = -x
  scalev n x = n * x
  checkBinRel Gt x y = x>y
  checkBinRel Lt x y = x<y
  checkBinRel Gteq x y = x >= y
  checkBinRel Lteq x y = x <= y
  checkBinRel Neq x y = not(x==y)
  checkBinRel Equal x y = x==y
  visZero x = x==0

instance Val t => Default Map t where
   apply mp k = case Map.lookup k mp of { Just t -> t; Nothing -> zeroV }

instance ProjectToBase b b where
   projectB x = x

instance (ProjectToBase b b, Val b) => ProjectToBase (Map k b) b where
   projectB x = findWithDefault zeroV adaTag x

instance (ProjectToBase (Map k b) b) => ProjectToBase (Map k (Map k b)) b where
   projectB = projectB . projectB 

instance (Ord k, Val t, NFData k, Show k, NoUnexpectedThunks k, Typeable k, Typeable t, ToCBOR t, FromCBOR t, ToCBOR k, FromCBOR k, ProjectToBase (Map k t) t) => Val (Map k t) where
  zeroV = Map.empty
  addv x y = unionWithV addv x y  -- There is an assumption that if the range is zeroV, it is not stored in the Map
  vnegate x = mapV vnegate x        -- We enforce this by using our own versions of map and union: unionWithV and mapV
  scalev n x = mapV (scalev n) x
  checkBinRel op x y = pointWise (checkBinRel op) x y
  visZero x = Map.null x
  -- getAdaAmount v = Coin $ c
  --   where
  --     c = apply v  -- foldl (+) 0 (fmap ((foldl (+) 0) . elems) (elems $ filterWithKey (\k _ -> k == adaID) v))

pointWise:: Ord k => (v -> v -> Bool) -> Map k v -> Map k v -> Bool
pointWise _ Tip Tip = True
pointWise _ Tip (Bin _ _ _ _ _) = False
pointWise _ (Bin _ _ _ _ _) Tip = False
pointWise p m (Bin _ k v2 ls rs) =
   case Map.splitLookup k m of
      (lm,Just v1,rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
      _ -> False


-- The following functions enforce the invariant that zeroV is never stored in a Map
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
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Ord, Integral, Real, Num, Enum, NoUnexpectedThunks, NFData, Val, Typeable)

-- | Asset ID
newtype AssetID = AssetID {assetID :: ByteString}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData, Typeable)

-- | Policy ID
newtype PolicyID crypto = PolicyID {policyID :: ScriptHash crypto}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData, Typeable)

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

-- | Value type
newtype Value crypto = Value
  { val :: Map  (PolicyID crypto) (Map AssetID Quantity) }
  deriving (Show, Generic, Val, Typeable)

instance NoUnexpectedThunks (Value crypto)
instance NFData (Value crypto)

-- | compact representation of Value
data CompactValue crypto = AdaOnly Coin | MixValue (Value crypto)
  deriving (Show, Eq, Generic, Typeable)

instance NoUnexpectedThunks (CompactValue crypto)
instance NFData (CompactValue crypto)

-- get the quantities of the tokens of a value term
getQs :: Value crypto -> [Quantity]
getQs (Value v) = fmap snd (concat $ fmap toList (elems v))

-- | make a Value out of a Coin
coinToValue :: Coin -> Value crypto
coinToValue (Coin c) = Value $ singleton adaID (singleton adaToken (Quantity c))

-- -- | add up all the ada in a Value
getAdaAmount :: Value crypto -> Coin
getAdaAmount (Value v) = Coin $ c
  where
    Quantity c = foldl (+) (Quantity 0) (getQs $ Value $ filterWithKey (\k _ -> k == adaID) v)

-- | policy ID of Ada
-- NOTE : this is an arbitrary choice of hash value - there is no known script that will
-- need to hash to this value. If a user finds one, there is still a check
-- in the STS rules to make sure it is not used
adaID :: PolicyID crypto
adaID = PolicyID $ ScriptHash $ coerce ("" :: Hash ShortHash (Script crypto))

-- | asset ID of Ada
-- this is an arbitrary choice of asset ID name!
adaToken :: AssetID
adaToken = AssetID $ "Ada"

-- | makes a compact representation of Value for in-memory storage
-- removes 0 values and adds a special case for ada-only values
valueToCompactValue :: Value crypto -> CompactValue crypto
valueToCompactValue vl@(Value v)
  | keys v == [adaID] = AdaOnly $ getAdaAmount vl
  | otherwise         = MixValue $ removeZeros v
    where
      removeZeros vv = Value $ fmap (filterWithKey (\_ q -> q /= (Quantity 0))) vv

-- returns the non-Ada tokens part of a Value token bundle
removeAda :: Value crypto -> Value crypto
removeAda (Value v) = Value $ filterWithKey (\k _ -> k /= adaID) v

-- | convert to Value
compactValueToValue :: CompactValue crypto -> Value crypto
compactValueToValue (AdaOnly c)  = coinToValue c
compactValueToValue (MixValue v) = v

instance (Typeable crypto, Crypto crypto) => Eq (Value crypto) where
    (==) = checkBinRel Equal

instance (Typeable crypto, Crypto crypto) => Semigroup (Value crypto) where
    (<>) = addv

instance (Typeable crypto, Crypto crypto) => Monoid (Value crypto) where
    mempty  = zeroV
    mappend = (<>)

-- instances for CompactValue
instance (Typeable crypto, Crypto crypto) => Semigroup (CompactValue crypto) where
    (<>) v1 v2 = valueToCompactValue $ addv (compactValueToValue v1) (compactValueToValue v2)

instance (Typeable crypto, Crypto crypto) => Monoid (CompactValue crypto) where
    mempty  = MixValue zeroV
    mappend = (<>)

-- constraint used for all parametrized functions
type CV c v = (Typeable c, Val v, Crypto c, Generic v, NoUnexpectedThunks v)

--
-- instance Group (Value crypto) where
--     inv = scale Integer (Value crypto) (-1)

-- deriving via (Additive (Value crypto)) instance AdditiveSemigroup (Value crypto)
-- deriving via (Additive (Value crypto)) instance AdditiveMonoid (Value crypto)
-- deriving via (Additive (Value crypto)) instance AdditiveGroup (Value crypto)

-- instance Module Integer (Value crypto) where
--     {-# INLINABLE scale #-}
--     scale i (Value xs) = Value (fmap (fmap (\i' -> i * i')) xs)

-- Linear Map instance


-- | Get the quantity of the given currency in the 'Value'.
valueOf :: Value crypto -> PolicyID crypto -> AssetID -> Quantity
valueOf (Value mp) cur tn =
    case Data.Map.Strict.lookup cur mp of
        Nothing -> (Quantity 0)
        Just i  -> case Data.Map.Strict.lookup tn i of
            Nothing -> (Quantity 0)
            Just v  -> v

-- | The list of 'PolicyID's of a 'Value'.
policyIDs :: Value crypto -> [PolicyID crypto]
policyIDs (Value mp) = keys mp

-- | Make a 'Value' containing only the given quantity of the given currency.
singleType :: PolicyID crypto -> AssetID -> Quantity -> Value crypto
singleType c tn i = Value (singleton c (singleton tn i))


-- Num operations

-- | subtract values
subv :: (Crypto crypto) => Value crypto -> Value crypto -> Value crypto
subv v1 v2 = addv v1 (vnegate v2)

vinsert:: (Crypto crypto) => PolicyID crypto -> AssetID -> Quantity -> Value crypto -> Value crypto
vinsert pid aid q old = addv old (Value (Map.singleton pid (Map.singleton aid q)))

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
splitValueFee :: Value crypto -> Integer -> (Value crypto, Coin)
splitValueFee (Value v) n
    | n <= 0 = error "must split coins into positive parts"
    | otherwise = (Value $ fmap (fmap (Quantity . (div n) . unInt)) v, getAdaAmount (Value v))

-- CBOR

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (CompactValue crypto)
 where
   toCBOR = \case
     AdaOnly c ->
           encodeListLen 2
           <> toCBOR (0 :: Word8)
           <> toCBOR c
     MixValue (Value v) ->
           encodeListLen 2
           <> toCBOR (1 :: Word8)
           <> toCBOR v

instance
  (Typeable crypto, Crypto crypto)
  => FromCBOR (CompactValue crypto)
 where
  fromCBOR = do
    decodeWord >>= \case
      0 -> do
        c <- fromCBOR
        pure $ AdaOnly c
      1 -> do
        v <- fromCBOR
        pure $ MixValue $ Value v
      k -> invalidKey k


instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (Value crypto)
 where
   toCBOR = (\case
     AdaOnly c ->
           encodeListLen 2
           <> toCBOR (0 :: Word8)
           <> toCBOR c
     MixValue (Value v) ->
           encodeListLen 2
           <> toCBOR (1 :: Word8)
           <> toCBOR v) . valueToCompactValue

instance
  (Typeable crypto, Crypto crypto)
  => FromCBOR (Value crypto)
 where
  fromCBOR = do
    decodeWord >>= \case
      0 -> do
        c <- fromCBOR
        pure $ compactValueToValue $ AdaOnly c
      1 -> do
        v <- fromCBOR
        pure $ compactValueToValue $ MixValue $ Value v
      k -> invalidKey k
