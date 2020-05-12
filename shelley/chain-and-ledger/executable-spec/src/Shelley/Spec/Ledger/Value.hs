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
import           Shelley.Spec.Ledger.Serialization (mapFromCBOR, mapToCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Data.Coerce (coerce)

import           Shelley.Spec.Ledger.BaseTypes (invalidKey)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Word (Word8)
import           Cardano.Crypto.Hash (hash, Hash, ShortHash(..))
import           Data.Map.Strict (Map, elems, empty, unionWith, toList, singleton, filterWithKey, keys, map,
                 toList, fromList, union, filter, singleton)
import           Shelley.Spec.Ledger.Crypto
import           Data.ByteString.Char8 (ByteString, pack) -- TODO needs to be 32 bytestring, 28 for pack
import           Shelley.Spec.Ledger.Scripts


-- | Quantity
newtype Quantity = Quantity Integer
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Ord, Integral, Num, Real, Enum, NoUnexpectedThunks)


-- | Value type
data Value crypto = Value
  { val :: Map (ScriptHash crypto) (Map ByteString Quantity) }
  deriving (Show, Eq, Generic)

data ValueBSType = ValueBSType (Map ByteString (Map ByteString Quantity))
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks ValueBSType
instance NoUnexpectedThunks (Value crypto)

-- | make a crypto-free Value type
-- TODO this is a hack!
toValBST :: Value crypto -> ValueBSType
toValBST (Value v) = ValueBSType $ fromList $ fmap (\(cid, tkns) -> (pack $ show cid, tkns)) (toList v)

-- | compact representation of Value
data CompactValue crypto = AdaOnly Coin | MixValue (Value crypto)
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (CompactValue crypto)

-- adding and subtracting values
-- TODO this must be an instance of something else - a Group?
-- needs + - and <=
instance (Crypto crypto) => Num (Value crypto) where
   (Value v1) + (Value v2) = Value (unionWith (unionWith (+)) v1 v2)
   (Value v1) - (Value v2) = Value (unionWith (unionWith (-)) v1 v2)
   (Value v1) * (Value v2) = Value (unionWith (unionWith (*)) v1 v2)
   abs (Value v1)          = Value (Data.Map.Strict.map (Data.Map.Strict.map abs) v1)
   signum v1
    | and $ fmap ((<) 0) (getQs v1) = 1
    | and $ fmap ((>) 0) (getQs v1) = -1
    | otherwise                     = 0
   fromInteger x           = Value $ singleton adaID (singleton adaToken (Quantity x))

-- Values are compared as functions that are 0 almost everywhere
instance (Crypto crypto) => Ord (Value crypto) where
   (<=) (Value v1) (Value v2) =
     and $ fmap ((<=) 0) (getQs $ (Value v1) - (Value v2))

-- get the quantities in the tokens of a value term
getQs :: Value crypto -> [Quantity]
getQs (Value v) = fmap snd (concat $ fmap toList (elems v))

-- zero value
zeroV :: Value crypto
zeroV = Value empty

-- | make a value out of a coin
coinToValue :: Crypto crypto => Coin -> Value crypto
coinToValue (Coin c) = Value $ singleton adaID (singleton adaToken (Quantity c))

-- | make a value out of a coin
getAdaAmount :: Crypto crypto => Value crypto -> Coin
getAdaAmount (Value v) = Coin $ c
  where
    Quantity c = foldl (+) (Quantity 0) (getQs $ Value $ filterWithKey (\k _ -> k == adaID) v)

-- | currency ID of Ada
-- TODO use the right script here
adaID :: Crypto crypto => ScriptHash crypto
adaID = ScriptHash $ coerce ("" :: Hash ShortHash (Script crypto))

-- | token of Ada
adaToken :: ByteString
adaToken = pack "Ada"

valueToCompactValue :: Crypto crypto => Value crypto -> CompactValue crypto
valueToCompactValue vl
  | keys v == [adaID] = AdaOnly $ getAdaAmount vl
  | otherwise         = MixValue $ uniqueAdaToken vl
    where
      (Value v) = vl

-- | make a Value term where the only token name within the ada currency is adaToken
uniqueAdaToken :: Crypto crypto => Value crypto -> Value crypto
uniqueAdaToken (Value v) = Value $ union (singleton adaID $ singleton adaToken $ foldl (+) 0
  (fmap snd $ concat $ fmap toList (elems (filterWithKey (\k _ -> k == adaID) v)))  )
  (filterWithKey (\k _ -> k /= adaID) v)

compactValueToValue :: Crypto crypto => CompactValue crypto -> Value crypto
compactValueToValue (AdaOnly c)  = coinToValue c
compactValueToValue (MixValue v) = v

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

instance ToCBOR ValueBSType where
  toCBOR (ValueBSType v) = mapToCBOR v

instance FromCBOR ValueBSType where
  fromCBOR = ValueBSType <$> mapFromCBOR
