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
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Data.Coerce (coerce)

import           Shelley.Spec.Ledger.BaseTypes (invalidKey)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Word (Word8)
import           Cardano.Crypto.Hash (Hash, ShortHash)
--import           Data.Group (Group (..))
--import           Data.Semigroup
--import           Data.Monoid
-- import           Algebra.Module
import           Data.Map.Strict (Map, elems, empty, toList, filterWithKey, keys,
                 toList, singleton, lookup, insert, unionWith)
import           Shelley.Spec.Ledger.Crypto
import           Data.ByteString.Char8 (ByteString, pack) -- TODO is this the right Bytestring 
import           Shelley.Spec.Ledger.Scripts




-- | Quantity
newtype Quantity = Quantity Integer
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Ord, Integral, Num, Real, Enum, NoUnexpectedThunks)

-- | Asset ID
newtype AssetID = AssetID ByteString
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks)

assetID :: AssetID -> ByteString
assetID (AssetID aid) = aid

-- | Policy ID
newtype PolicyID crypto = PolicyID (ScriptHash crypto)
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks)

policyID :: PolicyID crypto -> ScriptHash crypto
policyID (PolicyID pid) = pid

-- | A cryptocurrency value. This is a map from 'PolicyID's to a
-- quantity of assets with this policy.
--
-- Operations on assets are usually implemented /pointwise/. That is,
-- we apply the operation to the quantities for each asset in turn. So
-- when we add two 'Value's the resulting 'Value' has, for each asset,
-- the sum of the quantities of /that particular/ asset in the argument
-- 'Value'. The effect of this is that the assets in the 'Value' are "independent",
-- and are operated on separately.
--
-- Whenever we need to get the quantity of an asset in a 'Value' where there
-- is no explicit quantity of that asset in the 'Value', then the quantity is
-- taken to be zero.
--
-- See note [Assets] for more details.

-- | Value type
data Value crypto = Value
  { val :: Map  (PolicyID crypto) (Map AssetID Quantity) }
  deriving (Show, Generic)

instance NoUnexpectedThunks (Value crypto)

-- data ValueBSType = ValueBSType (Map ByteString (Map ByteString Quantity))
--   deriving (Show, Eq, Generic)
--
-- instance NoUnexpectedThunks ValueBSType
--
-- -- | make a crypto-free Value type
-- -- TODO this is a hack!
-- toValBST :: Value crypto -> ValueBSType
-- toValBST (Value v) = ValueBSType $ fromList $ fmap (\(cid, tkns) -> (pack $ show cid, tkns)) (toList v)

-- | compact representation of Value
data CompactValue crypto = AdaOnly Coin | MixValue (Value crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (CompactValue crypto)


-- get the quantities of the tokens of a value term
getQs :: Value crypto -> [Quantity]
getQs (Value v) = fmap snd (concat $ fmap toList (elems v))

-- zero value
zeroV :: Value crypto
zeroV = Value empty

-- | make a value out of a coin
coinToValue :: Coin -> Value crypto
coinToValue (Coin c) = Value $ singleton adaID (singleton adaToken (Quantity c))

-- | make a value out of a coin
getAdaAmount :: Value crypto -> Coin
getAdaAmount (Value v) = Coin $ c
  where
    Quantity c = foldl (+) (Quantity 0) (getQs $ Value $ filterWithKey (\k _ -> k == adaID) v)

-- | policy ID of Ada
adaID :: PolicyID crypto
adaID = PolicyID $ ScriptHash $ coerce ("" :: Hash ShortHash (Script crypto))

-- | asset ID of Ada
adaToken :: AssetID
adaToken = AssetID $ pack "Ada"

valueToCompactValue :: Value crypto -> CompactValue crypto
valueToCompactValue vl
  | keys v == [adaID] = AdaOnly $ getAdaAmount vl
  | otherwise         = MixValue $ uniqueAdaToken vl
    where
      (Value v) = vl

-- | make a Value term where the only token name within the ada asset group is adaToken
-- no 0-valued
uniqueAdaToken :: Value crypto -> Value crypto
uniqueAdaToken (Value v) = Value $ insert adaID
  (singleton adaToken $ foldl (+) 0
  (fmap snd $ concat $ fmap toList (elems (filterWithKey (\k _ -> k == adaID) v))) )
  (removeZeros $ filterWithKey (\k _ -> k /= adaID) v)
  where
    removeZeros vv = fmap (filterWithKey (\_ q -> q /= (Quantity 0))) vv

compactValueToValue :: CompactValue crypto -> Value crypto
compactValueToValue (AdaOnly c)  = coinToValue c
compactValueToValue (MixValue v) = v

instance Eq (Value crypto) where
    {-# INLINABLE (==) #-}
    (==) = eq

-- TODO these instances
-- No 'Ord Value' instance since 'Value' is only a partial order, so 'compare' can't
-- do the right thing in some cases.
-- Values are compared in a lexicographical way
-- this is needed to define an instance of Relation
-- use Map Ord instance
instance Ord (Value crypto) where
   compare (Value v1) (Value v2) = compare v1 v2

-- TODO this should return the same result as for Value
instance Ord (CompactValue crypto) where
   compare v1 v2 = compare (compactValueToValue v1) (compactValueToValue v2)

instance Semigroup (Value crypto) where
    {-# INLINABLE (<>) #-}
    (<>) = addv

instance Monoid (Value crypto) where
    {-# INLINABLE mempty #-}
    mempty  = zeroV
    mappend = (<>)
--
-- instance Group (Value crypto) where
--     {-# INLINABLE inv #-}
--     inv = scale Integer (Value crypto) (-1)

-- deriving via (Additive (Value crypto)) instance AdditiveSemigroup (Value crypto)
-- deriving via (Additive (Value crypto)) instance AdditiveMonoid (Value crypto)
-- deriving via (Additive (Value crypto)) instance AdditiveGroup (Value crypto)

-- instance Module Integer (Value crypto) where
--     {-# INLINABLE scale #-}
--     scale i (Value xs) = Value (fmap (fmap (\i' -> i * i')) xs)

-- Linear Map instance

{- note [Assets]

The 'Value' type represents a collection of amounts of different assets.

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


{-# INLINABLE valueOf #-}
-- | Get the quantity of the given currency in the 'Value'.
valueOf :: Value crypto -> PolicyID crypto -> AssetID -> Integer
valueOf (Value mp) cur tn =
    case Data.Map.Strict.lookup cur mp of
        Nothing -> 0 :: Integer
        Just i  -> case Data.Map.Strict.lookup tn i of
            Nothing -> 0
            Just (Quantity v)  -> v

{-# INLINABLE symbols #-}
-- | The list of 'PolicyID's of a 'Value'.
symbols :: Value crypto -> [PolicyID crypto]
symbols (Value mp) = keys mp

{-# INLINABLE singleType #-}
-- | Make a 'Value' containing only the given quantity of the given currency.
singleType :: PolicyID crypto -> AssetID -> Integer -> Value crypto
singleType c tn i = Value (singleton c (singleton tn (Quantity i)))

-- {-# INLINABLE unionVal #-}
-- -- | Combine two 'Value' maps
-- unionVal :: Value crypto -> Value crypto -> Map PolicyID (Map AssetID (These Integer Integer))
-- unionVal (Value l) (Value r) =
--     let
--         combined = Map.union l r
--         unThese k = case k of
--             This a    -> This <$> a
--             That b    -> That <$> b
--             These a b -> Map.union a b
--     in unThese <$> combined

-- {-# INLINABLE unionWith #-}
-- unionWith :: (Integer -> Integer -> Integer) -> Value crypto -> Value crypto -> Value crypto
-- unionWith f ls rs = zeroV -- TODO
--     -- let
--     --     combined = unionVal ls rs
--     --     unThese k' = case k' of
--     --         This a    -> f a 0
--     --         That b    -> f 0 b
--     --         These a b -> f a b
--     -- in Value (fmap (fmap unThese) combined)

-- Num operations

-- {-# INLINABLE isZero #-}
-- -- | Check whether a 'Value' is zero.
-- isZero :: Value crypto -> Bool
-- isZero (Value xs) = all (all (\i -> 0 == i)) xs

-- TODO
-- {-# INLINABLE checkPred #-}
-- checkPred :: (These Integer Integer -> Bool) -> Value crypto -> Value crypto -> Bool
-- checkPred f l r = True
--     let
--       inner :: Map.Map TokenName (These Integer Integer) -> Bool
--       inner = Map.all f
--     in
--       Map.all inner (unionVal l r)

{-# INLINABLE addv #-}
-- | add values
addv :: Value crypto -> Value crypto -> Value crypto
addv (Value v1) (Value v2) = Value (unionWith (unionWith (+)) v1 v2)

{-# INLINABLE subv #-}
-- | subtract values
subv :: Value crypto -> Value crypto -> Value crypto
subv (Value v1) (Value v2) = Value (unionWith (unionWith (-)) v1 v2)

{-# INLINABLE scalev #-}
-- | scale values
scalev :: Integer -> Value crypto -> Value crypto
scalev s (Value v) = Value (fmap (fmap ((Quantity s) *)) v)

{-# INLINABLE checkBinRel #-}
-- | Check whether a binary relation holds for value pairs of two 'Value' maps,
--   supplying 0 where a key is only present in one of them.
-- geq/leq/etc. only
-- TODO ^^
checkBinRel :: (Quantity -> Quantity -> Bool) -> Value crypto -> Value crypto -> Bool
checkBinRel f l r = and $ fmap (f 0) (getQs $ subv l r)


{-# INLINABLE geq #-}
-- | Check whether one 'Value' is greater than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
geq :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
geq = checkBinRel (>=)

{-# INLINABLE gt #-}
-- | Check whether one 'Value' is strictly greater than another. See 'Value' for an explanation of how operations on 'Value's work.
gt :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
gt = checkBinRel (>)

{-# INLINABLE leq #-}
-- | Check whether one 'Value' is less than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
leq :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
leq = checkBinRel (<=)

{-# INLINABLE lt #-}
-- | Check whether one 'Value' is strictly less than another. See 'Value' for an explanation of how operations on 'Value's work.
lt :: Value crypto -> Value crypto -> Bool
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
lt = checkBinRel (<)

{-# INLINABLE eq #-}
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

-- TODO do this right
splitValueFee :: Value crypto -> Integer -> (Value crypto, Coin)
splitValueFee v _ = (v, Coin 0)
-- (Value v) n =
--   | n <= 0 = error "must split coins into positive parts"
--   | otherwise = (fmap $ n `div` m, Coin $ n `rem` m)

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

-- instance ToCBOR ValueBSType where
--   toCBOR (ValueBSType v) = mapToCBOR v
--
-- instance FromCBOR ValueBSType where
--   fromCBOR = ValueBSType <$> mapFromCBOR
