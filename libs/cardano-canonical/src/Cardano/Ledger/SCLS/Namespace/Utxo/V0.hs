{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | UTxO namespace export.
module Cardano.Ledger.SCLS.Namespace.UTxO.V0 (
  UtxoKey (..),
  UtxoOut (..),
  CanonicalScript (..),
  IsCanonicalScript (..),
  CanonicalPlutusScript (..),
  IsCanonicalPlutusScript (..),
  CanonicalNativeScript (..),
  IsCanonicalNativeScript (..),
  CanonicalDatum (..),
  IsCanonicalDatum (..),
  CanonicalBabbageTxOut (..),
  IsCanonicalBabbageTxOut (..),
  CanonicalShelleyTxOut (..),
  IsCanonicalShelleyTxOut (..),
  CanonicalValue (..),
  IsCanonicalValue (..),
  PlutusBinary (..),
  CompactAddr,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.Binary (
  decodeMemPack,
  encodeMemPack,
  natVersion,
  toPlainDecoder,
  toPlainEncoding,
  toStrictByteString,
 )
import Cardano.Ledger.Hashes (
  DataHash,
  KeyHash (..),
  SafeHash,
  ScriptHash (..),
  Witness,
  originalBytes,
 )
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.SCLS.Common (CanonicalCoin (..), SlotNo (..))
import Cardano.Ledger.SCLS.LedgerCBOR (LedgerCBOR (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.SCLS.CBOR.Canonical (
  assumeCanonicalDecoder,
  assumeCanonicalEncoding,
 )
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonical,
  decodeListLenCanonicalOf,
  decodeMapLenCanonical,
  decodeWordCanonicalOf,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (
  ToCanonicalCBOR (..),
  encodeAsMap,
  mkEncodablePair,
 )
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import Data.ByteString (ByteString)
import Data.ByteString.Short
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..), isSNothing)
import Data.MemPack (MemPack (..), packByteStringM)
import Data.Sequence.Strict (StrictSeq)
import Data.Typeable
import Data.Word (Word8)
import GHC.Generics (Generic)

newtype MemPackCBOR a = MemPackCBOR {unMemPackCBOR :: a}
  deriving (Eq, Show)

instance MemPack a => ToCanonicalCBOR "utxo/v0" (MemPackCBOR a) where
  toCanonicalCBOR _v (MemPackCBOR a) = assumeCanonicalEncoding $ toPlainEncoding (natVersion @9) (encodeMemPack a)

instance MemPack a => FromCanonicalCBOR "utxo/v0" (MemPackCBOR a) where
  fromCanonicalCBOR =
    Versioned . MemPackCBOR
      <$> (assumeCanonicalDecoder $ toPlainDecoder Nothing (natVersion @9) decodeMemPack)

-- | Input wrapper for the keys that are used in utxo namespace
data UtxoKey
  = UtxoKeyIn TxIn
  deriving (Show)
  deriving (Generic)

instance Eq UtxoKey where
  UtxoKeyIn txIn1 == UtxoKeyIn txIn2 = txIn1 == txIn2

instance Ord UtxoKey where
  compare (UtxoKeyIn txIn1) (UtxoKeyIn txIn2) = compare txIn1 txIn2

instance IsKey UtxoKey where
  keySize = namespaceKeySize @"utxo/v0"
  packKeyM (UtxoKeyIn (TxIn (TxId a) b)) = do
    packByteStringM (originalBytes a)
    packM b
  unpackKeyM = do
    a <- unpackM -- FIXME read bytestirng and create unsafe hash
    b <- unpackM
    return $ UtxoKeyIn (TxIn a b)

{-
data CanonicalDatumHash = CanonicalDatumHash ShortByteString
  deriving (Eq, Show, Generic)

class IsCanonicalDatumHash a where
  mkCanonicalDatumHash :: a -> CanonicalDatumHash
  fromCanonicalDatumHash :: CanonicalDatumHash -> a
-}

data CanonicalShelleyTxOut = CanonicalShelleyTxOut
  { txOutCompactAddr :: {-# UNPACK #-} !CompactAddr
  , txOutCompactValue :: !CanonicalValue
  , txOutDatumHash :: !(StrictMaybe DataHash)
  }
  deriving (Eq, Show, Generic)

class IsCanonicalShelleyTxOut a where
  mkCanonicalShelleyTxOut :: a -> CanonicalShelleyTxOut
  fromCanonicalShelleyTxOut :: CanonicalShelleyTxOut -> a

data CanonicalValue
  = CanonicalValue CanonicalCoin (Map ScriptHash (Map ShortByteString CanonicalCoin))
  deriving (Show, Eq, Generic)

data CanonicalBabbageTxOut = CanonicalBabbageTxOut
  { babbageTxOutCompactAddr :: {-# UNPACK #-} !CompactAddr
  , babbageTxOutCompactValue :: !CanonicalValue
  , babbageTxOutDatum :: !CanonicalDatum
  , babbageTxOutRefScript :: !(StrictMaybe CanonicalScript)
  }
  deriving (Eq, Show, Generic)

class IsCanonicalBabbageTxOut a where
  mkCanonicalBabbageTxOut :: a -> CanonicalBabbageTxOut
  fromCanonicalBabbageTxOut :: CanonicalBabbageTxOut -> a

data CanonicalDatum
  = CanonicalNoDatum
  | CanonicalDatumHash !DataHash
  | CanonicalDatum !ByteString
  deriving (Eq, Show, Generic)

class IsCanonicalDatum a where
  mkCanonicalDatum :: a -> CanonicalDatum
  fromCanonicalDatum :: CanonicalDatum -> a

data CanonicalScript
  = CanonicalScriptNative !CanonicalNativeScript
  | CanonicalScriptPlutus !CanonicalPlutusScript
  deriving (Eq, Show, Generic)

class IsCanonicalScript a where
  mkCanonicalScript :: a -> CanonicalScript
  fromCanonicalScript :: CanonicalScript -> a

data CanonicalPlutusScript
  = CanonicalPlutusScriptV1 !PlutusBinary
  | CanonicalPlutusScriptV2 !PlutusBinary
  | CanonicalPlutusScriptV3 !PlutusBinary
  deriving (Eq, Show, Generic)

class IsCanonicalPlutusScript a where
  mkCanonicalPlutusScript :: a -> CanonicalPlutusScript
  fromCanonicalPlutusScript :: CanonicalPlutusScript -> a

data CanonicalNativeScript
  = CanonicalNativeScriptPubKey !(KeyHash Witness)
  | CanonicalNativeScriptAllOf (StrictSeq CanonicalNativeScript)
  | CanonicalNativeScriptAnyOf (StrictSeq CanonicalNativeScript)
  | CanonicalNativeScriptMOfN Int (StrictSeq CanonicalNativeScript)
  | CanonicalNativeScriptInvalidBefore !SlotNo
  | CanonicalNativeScriptInvalidAfter !SlotNo
  deriving (Eq, Show, Generic)

class IsCanonicalNativeScript a where
  mkCanonicalNativeScript :: a -> CanonicalNativeScript
  fromCanonicalNativeScript :: CanonicalNativeScript -> a

-- | Output key that is used in utxo namespace
--
-- Here we follow the current spec, but after benchmarks we can decide that this representation
-- is not efficient and we can replace it with the implementation based on the compact values
data UtxoOut
  = UtxoOutShelley CanonicalShelleyTxOut
  | UtxoOutBabbage CanonicalBabbageTxOut
  deriving (Show)
  deriving (Eq)
  deriving (Generic)

instance ToCanonicalCBOR "utxo/v0" UtxoOut where
  toCanonicalCBOR v (UtxoOutShelley shelleyOut) = toCanonicalCBOR v (0 :: Int, shelleyOut)
  toCanonicalCBOR v (UtxoOutBabbage babbageOut) = toCanonicalCBOR v (1 :: Int, babbageOut)

instance FromCanonicalCBOR "utxo/v0" UtxoOut where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (tag :: Int) <- fromCanonicalCBOR
    case tag of
      0 -> fmap UtxoOutShelley <$> fromCanonicalCBOR
      1 -> fmap UtxoOutBabbage <$> fromCanonicalCBOR
      _ -> fail "Invalid UtxoOut tag"

instance ToCanonicalCBOR "utxo/v0" CanonicalBabbageTxOut where
  toCanonicalCBOR v (CanonicalBabbageTxOut addr vl datum refScript) =
    encodeAsMap $
      [ mkEncodablePair v (0 :: Int) addr -- (compactAddr addr)
      , mkEncodablePair v (1 :: Int) (vl)
      ]
        <> foldMap
          (\d -> [mkEncodablePair v (2 :: Int) d])
          ( case datum of
              CanonicalNoDatum -> SNothing
              CanonicalDatumHash dh -> SJust (toCanonicalCBOR v (0 :: Int, originalBytes dh))
              CanonicalDatum binaryData -> SJust (toCanonicalCBOR v (1 :: Int, CBOR binaryData))
          )
        <> foldMap (\s -> [mkEncodablePair v (3 :: Int) s]) refScript

instance FromCanonicalCBOR "utxo/v0" CanonicalBabbageTxOut where
  fromCanonicalCBOR = do
    l <- decodeMapLenCanonical
    decodeWordCanonicalOf 0
    Versioned cAddr <- fromCanonicalCBOR @"utxo/v0"
    decodeWordCanonicalOf 1
    Versioned vl <- fromCanonicalCBOR @"utxo/v0"
    (datum, refScript) <-
      if l == 2
        then return (CanonicalNoDatum, SNothing)
        else do
          Versioned (n :: Int) <- fromCanonicalCBOR
          case n of
            2 -> do
              Versioned datum <- fromCanonicalCBOR @"utxo/v0"
              if l == 4
                then do
                  decodeWordCanonicalOf 3
                  Versioned script <- fromCanonicalCBOR @"utxo/v0"
                  return (datum, SJust script)
                else return (datum, SNothing)
            3 -> do
              Versioned script <- fromCanonicalCBOR @"utxo/v0"
              return (CanonicalNoDatum, SJust script)
            _ -> fail "Invalid Datum tag"
    return $ Versioned (CanonicalBabbageTxOut cAddr vl datum refScript)

class IsCanonicalValue a where
  mkCanonicalValue :: a -> CanonicalValue
  fromCanonicalValue :: CanonicalValue -> a

instance ToCanonicalCBOR "utxo/v0" CanonicalValue where
  toCanonicalCBOR v (CanonicalValue c m)
    | Map.null m = toCanonicalCBOR v c
    | otherwise = toCanonicalCBOR v (c, m)

instance FromCanonicalCBOR "utxo/v0" CanonicalValue where
  fromCanonicalCBOR = do
    len <- decodeListLenCanonical
    case len of
      1 -> do
        Versioned c <- fromCanonicalCBOR
        return $ Versioned (CanonicalValue c Map.empty)
      2 -> do
        Versioned c <- fromCanonicalCBOR
        Versioned m <- fromCanonicalCBOR
        return $ Versioned (CanonicalValue c m)
      _ -> fail "Invalid CanonicalValue length"

instance ToCanonicalCBOR "utxo/v0" CanonicalScript where
  toCanonicalCBOR v (CanonicalScriptNative s) = toCanonicalCBOR v (0 :: Word8, s)
  toCanonicalCBOR v (CanonicalScriptPlutus p) = toCanonicalCBOR v p

instance FromCanonicalCBOR "utxo/v0" CanonicalScript where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (w :: Word8) <- fromCanonicalCBOR
    case w of
      0 -> fmap (CanonicalScriptNative) <$> fromCanonicalCBOR
      1 -> fmap (CanonicalScriptPlutus . CanonicalPlutusScriptV1 . PlutusBinary) <$> fromCanonicalCBOR
      2 -> fmap (CanonicalScriptPlutus . CanonicalPlutusScriptV2 . PlutusBinary) <$> fromCanonicalCBOR
      3 -> fmap (CanonicalScriptPlutus . CanonicalPlutusScriptV3 . PlutusBinary) <$> fromCanonicalCBOR
      n -> fail ("Unknown tag: " <> show n)

instance ToCanonicalCBOR "utxo/v0" CanonicalNativeScript where
  toCanonicalCBOR v (CanonicalNativeScriptPubKey h) = toCanonicalCBOR v (0 :: Word8, h)
  toCanonicalCBOR v (CanonicalNativeScriptAllOf ns) = toCanonicalCBOR v (1 :: Word8, ns)
  toCanonicalCBOR v (CanonicalNativeScriptAnyOf ns) = toCanonicalCBOR v (2 :: Word8, ns)
  toCanonicalCBOR v (CanonicalNativeScriptMOfN m ns) = toCanonicalCBOR v (3 :: Word8, m, ns)
  toCanonicalCBOR v (CanonicalNativeScriptInvalidBefore slot) = toCanonicalCBOR v (4 :: Word8, slot)
  toCanonicalCBOR v (CanonicalNativeScriptInvalidAfter slot) = toCanonicalCBOR v (5 :: Word8, slot)

instance FromCanonicalCBOR "utxo/v0" CanonicalNativeScript where
  fromCanonicalCBOR = do
    l <- decodeListLenCanonical
    Versioned (n :: Word8) <- fromCanonicalCBOR @"utxo/v0"
    case n of
      0 -> fmap CanonicalNativeScriptPubKey <$> fromCanonicalCBOR @"utxo/v0"
      1 -> fmap CanonicalNativeScriptAllOf <$> fromCanonicalCBOR @"utxo/v0"
      2 -> fmap CanonicalNativeScriptAnyOf <$> fromCanonicalCBOR @"utxo/v0"
      3 | l == 3 -> do
        Versioned m <- fromCanonicalCBOR @"utxo/v0"
        Versioned ns <- fromCanonicalCBOR @"utxo/v0"
        return $ Versioned $ CanonicalNativeScriptMOfN m ns
      4 -> fmap CanonicalNativeScriptInvalidBefore <$> fromCanonicalCBOR @"utxo/v0"
      5 -> fmap CanonicalNativeScriptInvalidAfter <$> fromCanonicalCBOR @"utxo/v0"
      m -> fail $ "Invalid tag: " <> show m

instance ToCanonicalCBOR "utxo/v0" CanonicalShelleyTxOut where
  toCanonicalCBOR v CanonicalShelleyTxOut {..}
    | isSNothing txOutDatumHash = toCanonicalCBOR v (txOutCompactAddr, txOutCompactValue)
    | otherwise = toCanonicalCBOR v (txOutCompactAddr, txOutCompactValue, txOutDatumHash)

deriving via LedgerCBOR v (SafeHash s) instance ToCanonicalCBOR v (SafeHash s)

deriving via LedgerCBOR v (SafeHash s) instance Typeable s => FromCanonicalCBOR v (SafeHash s)

instance ToCanonicalCBOR "utxo/v0" CanonicalPlutusScript where
  toCanonicalCBOR v (CanonicalPlutusScriptV1 (PlutusBinary s)) = toCanonicalCBOR v (1 :: Word8, s)
  toCanonicalCBOR v (CanonicalPlutusScriptV2 (PlutusBinary s)) = toCanonicalCBOR v (2 :: Word8, s)
  toCanonicalCBOR v (CanonicalPlutusScriptV3 (PlutusBinary s)) = toCanonicalCBOR v (3 :: Word8, s)

instance FromCanonicalCBOR "utxo/v0" CanonicalPlutusScript where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (w :: Word8) <- fromCanonicalCBOR
    case w of
      1 -> fmap (CanonicalPlutusScriptV1 . PlutusBinary) <$> fromCanonicalCBOR
      2 -> fmap (CanonicalPlutusScriptV2 . PlutusBinary) <$> fromCanonicalCBOR
      3 -> fmap (CanonicalPlutusScriptV3 . PlutusBinary) <$> fromCanonicalCBOR
      n -> fail ("Unknown Plutus script version tag: " <> show n)

instance FromCanonicalCBOR "utxo/v0" CanonicalShelleyTxOut where
  fromCanonicalCBOR = do
    len <- decodeListLenCanonical
    case len of
      2 -> do
        Versioned addr <- fromCanonicalCBOR @"utxo/v0"
        Versioned vl <- fromCanonicalCBOR @"utxo/v0"
        return $ Versioned (CanonicalShelleyTxOut addr vl SNothing)
      3 -> do
        Versioned addr <- fromCanonicalCBOR @"utxo/v0"
        Versioned vl <- fromCanonicalCBOR @"utxo/v0"
        Versioned dh <- fromCanonicalCBOR @"utxo/v0"
        return $ Versioned (CanonicalShelleyTxOut addr vl dh)
      _ -> fail "Invalid CanonicalShelleyTxOut length"

deriving via MemPackCBOR CompactAddr instance ToCanonicalCBOR "utxo/v0" CompactAddr

deriving via MemPackCBOR CompactAddr instance FromCanonicalCBOR "utxo/v0" CompactAddr

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = UtxoKey
  type NamespaceEntry "utxo/v0" = UtxoOut

instance CanonicalCBOREntryEncoder "utxo/v0" UtxoOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"utxo/v0") n

instance CanonicalCBOREntryDecoder "utxo/v0" UtxoOut where
  decodeEntry = fromCanonicalCBOR

newtype CBOR = CBOR {unCBOR :: ByteString}
  deriving (Eq, Show)

instance ToCanonicalCBOR v CBOR where
  toCanonicalCBOR _v (CBOR a) = assumeCanonicalEncoding (E.encodePreEncoded a)

newtype PreEncoded = PreEncoded ByteString
  deriving (Eq, Show)

instance ToCanonicalCBOR v PreEncoded where
  toCanonicalCBOR _v (PreEncoded a) = assumeCanonicalEncoding (E.encodePreEncoded a)

instance FromCanonicalCBOR v PreEncoded where
  fromCanonicalCBOR = do
    fmap PreEncoded <$> fromCanonicalCBOR

instance FromCanonicalCBOR v CanonicalDatum where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned n <- fromCanonicalCBOR @"utxo/v0"
    case n of
      (0 :: Word8) -> do
        Versioned bs <- fromCanonicalCBOR
        return $ Versioned $ CanonicalDatumHash bs
      1 -> do
        24 <- assumeCanonicalDecoder $ D.decodeTag
        Versioned bs <- fromCanonicalCBOR @"utxo/v0"
        let bs' = toStrictByteString $ E.encodeTag 24 <> E.encodeBytes bs
        return $ Versioned $ CanonicalDatum bs'
      _ -> fail "Invalid CanonicalDatum tag"
