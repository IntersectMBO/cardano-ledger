{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts
  where

import           Cardano.Binary (ToCBOR, FromCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           GHC.Generics (Generic)
import           Cardano.Ledger.Shelley.Crypto
import           Keys (Hash)

import           Data.Word (Word8)
import           Cardano.Binary (Decoder, FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeBreakOr,
                     decodeListLen, decodeListLenOrIndef, decodeMapLenOrIndef, decodeWord,
                     encodeBreak, encodeListLen, encodeListLenIndef, encodeMapLen, encodeWord,
                     enforceSize, matchSize)
import           BaseTypes (CborSeq (..), UnitInterval, invalidKey)
import           Keys (AnyKeyHash, pattern AnyKeyHash)
import           Data.ByteString.Char8 (ByteString, pack)

import           Data.Map.Strict (Map, empty, unionWith)
import qualified Data.Map.Strict as Map
import           Numeric.Natural (Natural)


-- | A simple language for expressing conditions under which it is valid to
-- withdraw from a normal UTxO payment address or to use a stake address.
--
-- The use case is for expressing multi-signature payment addresses and
-- multi-signature stake addresses. These can be combined arbitrarily using
-- logical operations:
--
-- * multi-way \"and\";
-- * multi-way \"or\";
-- * multi-way \"N of M\".
--
-- This makes it easy to express multi-signature addresses, and provides an
-- extension point to express other validity conditions, e.g., as needed for
-- locking funds used with lightning.
--
data MultiSig crypto =
       -- | Require the redeeming transaction be witnessed by the spending key
       --   corresponding to the given verification key hash.
       RequireSignature   (AnyKeyHash crypto)

       -- | Require all the sub-terms to be satisfied.
     | RequireAllOf      [MultiSig crypto]

       -- | Require any one of the sub-terms to be satisfied.
     | RequireAnyOf      [MultiSig crypto]

       -- | Require M of the given sub-terms to be satisfied.
     | RequireMOf    Int [MultiSig crypto]
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (MultiSig crypto)

data Script crypto =
  MSig (MultiSig crypto) | SPLC (ScriptPLC crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (Script crypto)

data ScriptHash crypto =
  ScriptHashMSig (Hash (HASH crypto) (MultiSig crypto))
  | ScriptHashPLC (Hash (HASH crypto) (ScriptPLC crypto))
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (ScriptHash crypto)

-- | Tag
data IsThing = Yes | Nope
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks IsThing

-- | Validation tag
-- | this tag is for the creator of the block to add to every transaction
-- to mark whether the *scripts* in it validate (a transaction where not all
-- scripts validate can still be processed/pay fees)
newtype IsValidating = IsValidating IsThing
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord)

deriving instance ToCBOR IsValidating
deriving instance FromCBOR IsValidating

-- | For-fee tag
-- | this is a tag the author of the transactions must add to each input
-- which spends a non-script output to indicate whether it is used to
-- pay for script execution/transaction fees
newtype IsFee = IsFee IsThing
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord)

deriving instance ToCBOR IsFee
deriving instance FromCBOR IsFee

-- | This will be the Data type from the Plutus library
newtype DataHash crypto = DataHash (Hash (HASH crypto) (Data crypto))
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord)

deriving instance Crypto crypto => ToCBOR (DataHash crypto)
deriving instance Crypto crypto => FromCBOR (DataHash crypto)

-- | Plutus version
type PlutusVer = (Natural, Natural, Natural)

-- STAND-IN things!!
-- temp plc script! TODO should be Plutus script type instead of integer
data ScriptPLC crypto = ScriptPLC PlutusVer Integer
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (ScriptPLC crypto)

-- | Use these from Plutus
newtype Data crypto = Data Integer
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)

-- | Quantity
newtype Quantity = Quantity Integer
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Num, Ord, Real, Integral, Enum, NoUnexpectedThunks)


-- | Value represents a collection of tokens/currencies
-- the ScriptHash parameter is the hash of the script used to model the
-- monetary policy of the currency
data Value crypto = Value (Map (ScriptHash crypto) (Map ByteString Quantity))
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (Value crypto)

-- | Overloaded functions for operations on underlying numeric Quantity in
-- | the Value type (in a scalar way)
instance Num (Value crypto) where
   (Value v1) + (Value v2) = Value ((unionWith (unionWith (+))) v1 v2)
   (Value v1) * (Value v2) = Value ((unionWith (unionWith (*))) v1 v2)
   (Value v1) - (Value v2) = Value ((unionWith (unionWith (-))) v1 v2)

-- | CBOR temp
instance
  (Crypto crypto)
  => ToCBOR (ScriptPLC crypto)
 where
  toCBOR (ScriptPLC pv scr) =
      encodeListLen 2
       <> toCBOR pv
       <> toCBOR scr

instance (Crypto crypto) =>
  FromCBOR (ScriptPLC crypto) where
  fromCBOR = do
    enforceSize "ScriptPLC" 2
    a <- fromCBOR
    b <- fromCBOR
    pure $ ScriptPLC a b

instance
  (Crypto crypto)
  => ToCBOR (Value crypto)
  where
    toCBOR (Value v) = toCBOR v

instance
  Crypto crypto
  => FromCBOR (Value crypto)
 where
  fromCBOR = do
    v <- fromCBOR
    pure $ (Value v)

instance
  ToCBOR IsThing
 where
  toCBOR = \case
    Yes  -> toCBOR (0 :: Word8)
    Nope -> toCBOR (1 :: Word8)

instance
  FromCBOR IsThing
 where
  fromCBOR = enforceSize "IsThing" 1  >> decodeWord >>= \case
    0 -> do
      pure $ Yes
    1 -> do
      pure $ Nope
    k -> invalidKey k

instance
  (Crypto crypto)
  => ToCBOR (ScriptHash crypto)
 where
  toCBOR = \case
    ScriptHashMSig hs ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR hs

    ScriptHashPLC hs ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR hs

instance
  (Crypto crypto)
  => ToCBOR (Script crypto)
 where
  toCBOR = \case
    MSig ms ->
      encodeListLen 2
       <> toCBOR (0 :: Word8)
       <> toCBOR ms

    SPLC plc ->
      encodeListLen 2
       <> toCBOR (1 :: Word8)
       <> toCBOR plc

instance
  Crypto crypto
  => FromCBOR (Script crypto)
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "MSig" 1 n
        a <- fromCBOR
        pure $ MSig a
      0 -> do
        matchSize "SPLC" 1 n
        a <- fromCBOR
        pure $ SPLC a
      k -> invalidKey k


instance (Crypto crypto) =>
  FromCBOR (ScriptHash crypto) where
  fromCBOR = enforceSize "ScriptHash" 2  >> decodeWord >>= \case
    0 -> do
      a <- fromCBOR
      pure $ ScriptHashMSig a
    1 -> do
      a <- fromCBOR
      pure $ ScriptHashPLC a
    k -> invalidKey k

instance (Crypto crypto) =>
  ToCBOR (MultiSig crypto) where
  toCBOR (RequireSignature hk) =
    encodeListLen 2 <> encodeWord 0 <> toCBOR hk
  toCBOR (RequireAllOf msigs) =
    encodeListLen 2 <> encodeWord 1 <> toCBOR msigs
  toCBOR (RequireAnyOf msigs) =
    encodeListLen 2 <> encodeWord 2 <> toCBOR msigs
  toCBOR (RequireMOf m msigs) =
    encodeListLen 3 <> encodeWord 3 <> toCBOR m <> toCBOR msigs

instance (Crypto crypto) =>
  FromCBOR (MultiSig crypto) where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "RequireSignature" 2 n >> (RequireSignature . AnyKeyHash) <$> fromCBOR
      1 -> matchSize "RequireAllOf" 2 n >> RequireAllOf <$> fromCBOR
      2 -> matchSize "RequireAnyOf" 2 n >> RequireAnyOf <$> fromCBOR
      3 -> do
        matchSize "RequireMOf" 3 n
        m     <- fromCBOR
        msigs <- fromCBOR
        pure $ RequireMOf m msigs
      k -> invalidKey k
