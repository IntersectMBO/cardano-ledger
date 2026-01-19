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
{-# OPTIONS_GHC -Wno-orphans -Werror #-}

-- | UTxO namespace export.
module Cardano.Ledger.SCLS.Namespace.UTxO.V0 (
  UtxoKey (..),
  UtxoOut (..),
  CanonicalScript(..),
  IsCanonicalScript(..),
  CanonicalPlutusScript(..),
  IsCanonicalPlutusScript(..),
  CanonicalNativeScript(..),
  IsCanonicalNativeScript(..),
  CanonicalDatum(..),
  IsCanonicalDatum (..),
  CanonicalBabbageTxOut (..),
  IsCanonicalBabbageTxOut (..),
  CanonicalShelleyTxOut (..),
  IsCanonicalShelleyTxOut (..),
) where

import Cardano.Ledger.Address
--import Cardano.Ledger.Allegra.Scripts (Timelock (..), TimelockRaw (..))
{-
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  decodePlutusScript,
 )
-}
--import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Binary (
  decodeMemPack,
  encodeMemPack,
  natVersion,
  toPlainDecoder,
  toPlainEncoding,
 )
-- import Cardano.Ledger.Compactible (CompactForm (..))
-- import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.SCLS.Common (SlotNo(..), CanonicalCoin(..))
import Cardano.Ledger.SCLS.LedgerCBOR (LedgerCBOR (..))
-- import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Hashes (originalBytes, {- KeyHash(..),-} DataHash, ScriptHash(..), SafeHash)
-- import Cardano.Ledger.Mary (MaryValue)
-- import Cardano.Ledger.MemoBytes (mkMemoizedEra)
-- import Cardano.Ledger.Plutus.Data (BinaryData, Datum (..))
-- import Cardano.Ledger.Plutus.Language (SLanguage (..))
-- import qualified Cardano.Ledger.Shelley.TxOut as Shelley
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
import Data.ByteString.Short
import Data.Maybe.Strict (StrictMaybe (..), isSNothing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.MemPack (MemPack (..), packByteStringM)
import qualified Data.Text as T
-- import Data.Proxy (Proxy (..))
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
  = CanonicalValue CanonicalCoin (Map ScriptHash (Map T.Text CanonicalCoin))
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
  | CanonicalDatum !ShortByteString
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
  = CanonicalPlutusScriptV1 !ShortByteString
  | CanonicalPlutusScriptV2 !ShortByteString
  | CanonicalPlutusScriptV3 !ShortByteString
  deriving (Eq, Show, Generic)

class IsCanonicalPlutusScript a where
  mkCanonicalPlutusScript :: a -> CanonicalPlutusScript
  fromCanonicalPlutusScript :: CanonicalPlutusScript -> a

data CanonicalNativeScript
  = CanonicalNativeScriptPubKey !ScriptHash
  | CanonicalNativeScriptAllOf [CanonicalNativeScript]
  | CanonicalNativeScriptAnyOf [CanonicalNativeScript]
  | CanonicalNativeScriptMOfN Int [CanonicalNativeScript]
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
              CanonicalDatum binaryData -> SJust (toCanonicalCBOR v (1 :: Int, toCanonicalCBOR v (LedgerCBOR @"utxo/v0" binaryData)))
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
                  return (CanonicalDatumHash datum, SJust script)
                else return (CanonicalDatumHash datum, SNothing)
            3 -> do
              Versioned script <- fromCanonicalCBOR @"utxo/v0"
              return (CanonicalNoDatum, SJust script)
            _ -> fail "Invalid Datum tag"
    return $ Versioned (CanonicalBabbageTxOut cAddr vl datum refScript)

instance ToCanonicalCBOR "utxo/v0" CanonicalValue where
  toCanonicalCBOR v (CanonicalValue c m)
     | Map.null m = toCanonicalCBOR v [c]
     | otherwise  = toCanonicalCBOR v (c, m)
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
  toCanonicalCBOR v (CanonicalScriptNative s) = toCanonicalCBOR v (0::Word8, s)
  toCanonicalCBOR v (CanonicalScriptPlutus p) = toCanonicalCBOR v p

instance FromCanonicalCBOR "utxo/v0" CanonicalScript where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (w :: Word8) <- fromCanonicalCBOR
    case w of
      0 -> fmap (CanonicalScriptNative) <$> fromCanonicalCBOR
      1 ->
        fmap (CanonicalScriptPlutus . CanonicalPlutusScriptV1) <$> fromCanonicalCBOR
          -- <$> ( assumeCanonicalDecoder $
          --         toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV1)
          --     )
      2 ->
        fmap (CanonicalScriptPlutus . CanonicalPlutusScriptV2) <$> fromCanonicalCBOR
          -- <$> ( assumeCanonicalDecoder $
          --         toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV2)
          --     )
      3 ->
        fmap (CanonicalScriptPlutus . CanonicalPlutusScriptV3) <$> fromCanonicalCBOR
          -- <$> ( assumeCanonicalDecoder $
          --         toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV3)
          --     )
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
    decodeListLenCanonicalOf 2
    Versioned (n :: Word8) <- fromCanonicalCBOR @"utxo/v0"
    case n of
      0 -> fmap CanonicalNativeScriptPubKey <$> fromCanonicalCBOR @"utxo/v0"
      1 -> fmap CanonicalNativeScriptAllOf <$> fromCanonicalCBOR @"utxo/v0"
      2 -> fmap CanonicalNativeScriptAnyOf <$> fromCanonicalCBOR @"utxo/v0"
      3 -> do
        decodeListLenCanonicalOf 3
        Versioned m <- fromCanonicalCBOR @"utxo/v0"
        Versioned ns <- fromCanonicalCBOR @"utxo/v0"
        return $ Versioned $ CanonicalNativeScriptMOfN m ns
      4 -> fmap CanonicalNativeScriptInvalidBefore <$> fromCanonicalCBOR @"utxo/v0"
      5 -> fmap CanonicalNativeScriptInvalidAfter <$> fromCanonicalCBOR @"utxo/v0"
      m -> fail $ "Invalid tag: " <> show m

instance ToCanonicalCBOR "utxo/v0" CanonicalShelleyTxOut where
  toCanonicalCBOR v CanonicalShelleyTxOut{..}
    | isSNothing txOutDatumHash = toCanonicalCBOR v (txOutCompactAddr, txOutCompactValue)
    | otherwise = toCanonicalCBOR v (txOutCompactAddr, txOutCompactValue, txOutDatumHash)

deriving via LedgerCBOR v (SafeHash s) instance ToCanonicalCBOR v (SafeHash s)

deriving via LedgerCBOR v (SafeHash s) instance Typeable s => FromCanonicalCBOR v (SafeHash s)

instance ToCanonicalCBOR "utxo/v0" CanonicalPlutusScript where
  toCanonicalCBOR v (CanonicalPlutusScriptV1 s) = toCanonicalCBOR v (1 :: Word8, s)
  toCanonicalCBOR v (CanonicalPlutusScriptV2 s) = toCanonicalCBOR v (2 :: Word8, s)
  toCanonicalCBOR v (CanonicalPlutusScriptV3 s) = toCanonicalCBOR v (3 :: Word8, s)


instance FromCanonicalCBOR "utxo/v0" CanonicalPlutusScript where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (w :: Word8) <- fromCanonicalCBOR
    case w of
      1 -> fmap CanonicalPlutusScriptV1 <$> fromCanonicalCBOR
      2 -> fmap CanonicalPlutusScriptV2 <$> fromCanonicalCBOR
      3 -> fmap CanonicalPlutusScriptV3 <$> fromCanonicalCBOR
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

{-
deriving via
  LedgerCBORSafe v (Shelley.ShelleyTxOut ConwayEra)
  instance
    FromCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)
-}



{-
deriving via
  LedgerCBOR v (AlonzoScript ConwayEra)
  instance
    ToCanonicalCBOR v (AlonzoScript ConwayEra)
-}

{-
instance FromCanonicalCBOR v (AlonzoScript ConwayEra) where

-}

{-
instance FromCanonicalCBOR v (Timelock ConwayEra) where
  fromCanonicalCBOR = do
    Versioned raw <- fromCanonicalCBOR
    return $ Versioned $ mkMemoizedEra @ConwayEra (raw :: TimelockRaw ConwayEra)
-}

{-
instance FromCanonicalCBOR v (TimelockRaw ConwayEra) where
  fromCanonicalCBOR = do
    k <- decodeListLenCanonical
    Versioned (n :: Word8) <- fromCanonicalCBOR
    case n of
      0 -> fmap TimelockSignature <$> fromCanonicalCBOR
      1 -> fmap TimelockAllOf <$> fromCanonicalCBOR
      2 -> fmap TimelockAnyOf <$> fromCanonicalCBOR
      3 | k == 3 -> do
        Versioned f <- fromCanonicalCBOR
        Versioned g <- fromCanonicalCBOR
        return $ Versioned $ TimelockMOf f g
      4 -> fmap TimelockTimeStart <$> fromCanonicalCBOR
      5 -> fmap TimelockTimeExpire <$> fromCanonicalCBOR
      m -> fail $ "Invalid tag: " <> show m
-}

-- deriving via (LedgerCBORSafe v MaryValue) instance ToCanonicalCBOR v MaryValue

-- deriving via (LedgerCBORSafe v MaryValue) instance FromCanonicalCBOR v MaryValue

{-
instance ToCanonicalCBOR version (CompactForm MaryValue) where
  toCanonicalCBOR version v = toCanonicalCBOR version (fromCompact v)

instance FromCanonicalCBOR version (CompactForm MaryValue) where
  fromCanonicalCBOR = do
    Versioned v <- fromCanonicalCBOR
    Just v' <- pure (toCompact v)
    pure $ Versioned v'
-}

deriving via MemPackCBOR CompactAddr instance ToCanonicalCBOR "utxo/v0" CompactAddr

deriving via MemPackCBOR CompactAddr instance FromCanonicalCBOR "utxo/v0" CompactAddr

-- deriving via LedgerCBOR v (Timelock ConwayEra) instance ToCanonicalCBOR v (Timelock ConwayEra)

-- deriving via LedgerCBOR v (Datum ConwayEra) instance ToCanonicalCBOR v (Datum ConwayEra)

-- deriving via LedgerCBOR v (Datum ConwayEra) instance FromCanonicalCBOR v (Datum ConwayEra)

-- deriving via LedgerCBOR v (BinaryData ConwayEra) instance ToCanonicalCBOR v (BinaryData ConwayEra)

{-
deriving via
  LedgerCBOR v (BinaryData ConwayEra)
  instance
    FromCanonicalCBOR v (BinaryData ConwayEra)
-}

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = UtxoKey
  type NamespaceEntry "utxo/v0" = UtxoOut

instance CanonicalCBOREntryEncoder "utxo/v0" UtxoOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"utxo/v0") n

instance CanonicalCBOREntryDecoder "utxo/v0" UtxoOut where
  decodeEntry = fromCanonicalCBOR
