{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Shelley.Spec.Ledger.Value
 where

import           Cardano.Binary (ToCBOR, FromCBOR, toCBOR, fromCBOR, encodeListLen,
                  decodeWord)
import           Cardano.Prelude (NoUnexpectedThunks(..), NFData ())
import           Data.Coerce (coerce)

import           Shelley.Spec.Ledger.BaseTypes (invalidKey)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Word (Word8)
import           Cardano.Crypto.Hash (Hash, ShortHash)
import           Data.Map.Strict (Map, elems, empty, toList, filterWithKey, keys,
                 toList, singleton, lookup, unionWith)
import           Shelley.Spec.Ledger.Crypto
import           Data.ByteString (ByteString) -- TODO is this the right Bytestring
import           Shelley.Spec.Ledger.Scripts




-- | Quantity
newtype Quantity = Quantity {unInt :: Integer}
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Ord, Integral, Real, Num, Enum, NoUnexpectedThunks, NFData)

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

-- | Value type
data Value crypto = Value
  { val :: Map  (PolicyID crypto) (Map AssetID Quantity) }
  deriving (Show, Generic)

instance NoUnexpectedThunks (Value crypto)
instance NFData (Value crypto)

-- | compact representation of Value
data CompactValue crypto = AdaOnly Coin | MixValue (Value crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (CompactValue crypto)
instance NFData (CompactValue crypto)

-- get the quantities of the tokens of a value term
getQs :: Value crypto -> [Quantity]
getQs (Value v) = fmap snd (concat $ fmap toList (elems v))

-- zero value
zeroV :: Value crypto
zeroV = Value empty

-- | make a Value out of a Coin
coinToValue :: Coin -> Value crypto
coinToValue (Coin c) = Value $ singleton adaID (singleton adaToken (Quantity c))

-- | add up all the ada in a Value
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

-- | convert to Value
compactValueToValue :: CompactValue crypto -> Value crypto
compactValueToValue (AdaOnly c)  = coinToValue c
compactValueToValue (MixValue v) = v

instance Eq (Value crypto) where
    (==) = eq


instance Semigroup (Value crypto) where
    (<>) = addv

instance Monoid (Value crypto) where
    mempty  = zeroV
    mappend = (<>)

-- instances for CompactValue
instance Semigroup (CompactValue crypto) where
    (<>) v1 v2 = valueToCompactValue $ addv (compactValueToValue v1) (compactValueToValue v2)

instance Monoid (CompactValue crypto) where
    mempty  = MixValue zeroV
    mappend = (<>)

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

-- | Check whether a 'Value' is zero.
isZero :: Value crypto -> Bool
isZero (Value xs) = all (all (\i -> 0 == i)) xs

-- | add values
addv :: Value crypto -> Value crypto -> Value crypto
addv (Value v1) (Value v2) = Value (unionWith (unionWith (+)) v1 v2)

-- | subtract values
subv :: Value crypto -> Value crypto -> Value crypto
subv (Value v1) (Value v2) = Value (unionWith (unionWith (-)) v1 v2)

-- | scale values
scalev :: Integer -> Value crypto -> Value crypto
scalev s (Value v) = Value (fmap (fmap ((Quantity s) *)) v)

-- | Check whether a binary relation holds for value pairs of two 'Value' maps,
--   supplying 0 where a key is only present in one of them.
-- geq/leq/etc. only
-- TODO ^^
checkBinRel :: (Quantity -> Quantity -> Bool) -> Value crypto -> Value crypto -> Bool
checkBinRel f l r = and $ fmap (f 0) (getQs $ subv l r)


-- | Check whether one 'Value' is greater than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
geq :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
geq = checkBinRel (>=)

-- | Check whether one 'Value' is strictly greater than another. See 'Value' for an explanation of how operations on 'Value's work.
gt :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
gt = checkBinRel (>)

-- | Check whether one 'Value' is less than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
leq :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
leq = checkBinRel (<=)

-- | Check whether one 'Value' is strictly less than another. See 'Value' for an explanation of how operations on 'Value's work.
lt :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
lt = checkBinRel (<)

-- | Check whether one 'Value' is equal to another. See 'Value' for an explanation of how operations on 'Value's work.
eq :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
eq = checkBinRel (==)

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
  (Crypto crypto)
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
  (Crypto crypto)
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
  (Crypto crypto)
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
  (Crypto crypto)
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
