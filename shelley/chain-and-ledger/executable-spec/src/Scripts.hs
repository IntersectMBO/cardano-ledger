{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts
  where

import           Cardano.Binary (ToCBOR, FromCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
-- import           Coin (Coin (..))
import           GHC.Generics (Generic)
-- import           Data.Map
import           Cardano.Ledger.Shelley.Crypto
import           Keys (Hash)
--import           CostModel

import           Data.Word (Word8)
import           Cardano.Binary (Decoder, FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeBreakOr,
                     decodeListLen, decodeListLenOrIndef, decodeMapLenOrIndef, decodeWord,
                     encodeBreak, encodeListLen, encodeListLenIndef, encodeMapLen, encodeWord,
                     enforceSize, matchSize)
import           BaseTypes (CborSeq (..), UnitInterval, invalidKey)
--import           Serialization (CBORGroup (..), FromCBORGroup (..), ToCBORGroup (..))
import           Keys (AnyKeyHash, pattern AnyKeyHash) --, GenKeyHash, Hash, KeyHash, pattern KeyHash,
-- --                     Sig, VKey, VKeyGenesis, VerKeyVRF, hashAnyKey, hash)

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
newtype IsValidating = IsValidating IsThing
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)

-- | For-fee tag
newtype IsFee = IsFee IsThing
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)

newtype DataHash crypto = DataHash (Hash (HASH crypto) (Data crypto))
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord)

deriving instance Crypto crypto => ToCBOR (DataHash crypto)
deriving instance Crypto crypto => FromCBOR (DataHash crypto)

-- STAND-IN things!!
-- temp plc script! Use these from Plutus
newtype ScriptPLC crypto = ScriptPLC Integer
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)


-- | Use these from Plutus
newtype Data crypto = Data Integer
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)

-- | Quantity
newtype Quantity = Quantity Natural
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Num, Ord, Real, Integral, Enum, NoUnexpectedThunks)


-- | Value type
data Value crypto = Value (Map (ScriptHash crypto) (Map String Quantity))
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
  => ToCBOR (Value crypto)
  where
    toCBOR v = toCBOR (1 :: Word8)

instance
  Crypto crypto
  => FromCBOR (Value crypto)
 where
  fromCBOR =
      pure $ (Value empty)

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
  fromCBOR = enforceSize "Script" 2  >> decodeWord >>= \case
    0 -> do
      a <- fromCBOR
      pure $ MSig a
    1 -> do
      a <- fromCBOR
      pure $ SPLC a
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
