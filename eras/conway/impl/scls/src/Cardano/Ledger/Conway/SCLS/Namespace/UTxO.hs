{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Werror #-}

-- | UTxO namespace export.
module Cardano.Ledger.Conway.SCLS.Namespace.UTxO (
  UtxoKey (..),
  UtxoOut (..),

  module Cardano.Ledger.SCLS.Namespace.UTxO.V0,
) where

import Cardano.Ledger.SCLS.Namespace.UTxO.V0
{-
import Cardano.Ledger.Address
-}
import Cardano.Ledger.Allegra.Scripts (Timelock (..){-, TimelockRaw (..)-})
{-
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  decodePlutusScript,
 )
-}
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
{-
import Cardano.Ledger.Binary (
  decodeMemPack,
  encodeMemPack,
  natVersion,
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.Compactible (CompactForm (..))
-}
import Cardano.Ledger.Conway (ConwayEra)
{-
import Cardano.Ledger.SCLS.Common ()
import Cardano.Ledger.SCLS.LedgerCBOR (LedgerCBOR (..), LedgerCBORSafe (..))
-}
import Cardano.Ledger.Conway.Scripts (AlonzoScript(..), PlutusScript(..))
{-
import Cardano.Ledger.Hashes (originalBytes)
import Cardano.Ledger.Mary (MaryValue)
-}
-- import Cardano.Ledger.MemoBytes (mkMemoizedEra, getMemoRawType)
import Cardano.Ledger.Plutus.Data ({-BinaryData,-} Datum (..))
{-
import Cardano.Ledger.Plutus.Language (SLanguage (..))
-}
import qualified Cardano.Ledger.Shelley.TxOut as Shelley
{-
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
import Data.Maybe.Strict (StrictMaybe (..))
import Data.MemPack (MemPack (..), packByteStringM)
import Data.Proxy (Proxy (..))
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

-- | Output key that is used in utxo namespace
--
-- Here we follow the current spec, but after benchmarks we can decide that this representation
-- is not efficient and we can replace it with the implementation based on the compact values
data UtxoOut
  = UtxoOutShelley (Shelley.ShelleyTxOut ConwayEra)
  | UtxoOutBabbage (Babbage.BabbageTxOut ConwayEra)
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

instance ToCanonicalCBOR "utxo/v0" (Babbage.BabbageTxOut ConwayEra) where
  toCanonicalCBOR v (Babbage.BabbageTxOut addr vl datum refScript) =
    encodeAsMap $
      [ mkEncodablePair v (0 :: Int) (compactAddr addr)
      , mkEncodablePair v (1 :: Int) (vl)
      ]
        <> foldMap
          (\d -> [mkEncodablePair v (2 :: Int) d])
          ( case datum of
              NoDatum -> SNothing
              DatumHash dh -> SJust (toCanonicalCBOR v (0 :: Int, originalBytes dh))
              Datum binaryData -> SJust (toCanonicalCBOR v (1 :: Int, toCanonicalCBOR v (LedgerCBOR @"utxo/v0" binaryData)))
          )
        <> foldMap (\s -> [mkEncodablePair v (3 :: Int) s]) refScript

instance FromCanonicalCBOR "utxo/v0" (Babbage.BabbageTxOut ConwayEra) where
  fromCanonicalCBOR = do
    l <- decodeMapLenCanonical
    decodeWordCanonicalOf 0
    Versioned cAddr <- fromCanonicalCBOR @"utxo/v0"
    decodeWordCanonicalOf 1
    Versioned vl <- fromCanonicalCBOR
    (datum, refScript) <-
      if l == 2
        then return (NoDatum, SNothing)
        else do
          Versioned (n :: Int) <- fromCanonicalCBOR
          case n of
            2 -> do
              Versioned datum <- fromCanonicalCBOR
              if l == 4
                then do
                  decodeWordCanonicalOf 3
                  Versioned script <- fromCanonicalCBOR @"utxo/v0"
                  return (datum, SJust script)
                else return (datum, SNothing)
            3 -> do
              Versioned script <- fromCanonicalCBOR @"utxo/v0"
              return (NoDatum, SJust script)
            _ -> fail "Invalid Datum tag"
    return $ Versioned (Babbage.BabbageTxOut (decompactAddr cAddr) vl datum refScript)

deriving via
  LedgerCBORSafe v (Shelley.ShelleyTxOut ConwayEra)
  instance
    ToCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)

deriving via
  LedgerCBORSafe v (Shelley.ShelleyTxOut ConwayEra)
  instance
    FromCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)

deriving via
  LedgerCBOR v (AlonzoScript ConwayEra)
  instance
    ToCanonicalCBOR v (AlonzoScript ConwayEra)

instance FromCanonicalCBOR v (AlonzoScript ConwayEra) where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (w :: Word8) <- fromCanonicalCBOR
    case w of
      0 -> fmap (NativeScript) <$> fromCanonicalCBOR
      1 ->
        Versioned . PlutusScript
          <$> ( assumeCanonicalDecoder $
                  toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV1)
              )
      2 ->
        Versioned . PlutusScript
          <$> ( assumeCanonicalDecoder $
                  toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV2)
              )
      3 ->
        Versioned . PlutusScript
          <$> ( assumeCanonicalDecoder $
                  toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV3)
              )
      n -> fail ("Unknown tag: " <> show n)

instance FromCanonicalCBOR v (Timelock ConwayEra) where
  fromCanonicalCBOR = do
    Versioned raw <- fromCanonicalCBOR
    return $ Versioned $ mkMemoizedEra @ConwayEra (raw :: TimelockRaw ConwayEra)

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

deriving via (LedgerCBORSafe v MaryValue) instance ToCanonicalCBOR v MaryValue

deriving via (LedgerCBORSafe v MaryValue) instance FromCanonicalCBOR v MaryValue

instance ToCanonicalCBOR version (CompactForm MaryValue) where
  toCanonicalCBOR version v = toCanonicalCBOR version (fromCompact v)

instance FromCanonicalCBOR version (CompactForm MaryValue) where
  fromCanonicalCBOR = do
    Versioned v <- fromCanonicalCBOR
    Just v' <- pure (toCompact v)
    pure $ Versioned v'

deriving via MemPackCBOR CompactAddr instance ToCanonicalCBOR "utxo/v0" CompactAddr

deriving via MemPackCBOR CompactAddr instance FromCanonicalCBOR "utxo/v0" CompactAddr

deriving via LedgerCBOR v (Timelock ConwayEra) instance ToCanonicalCBOR v (Timelock ConwayEra)

deriving via LedgerCBOR v (Datum ConwayEra) instance ToCanonicalCBOR v (Datum ConwayEra)

deriving via LedgerCBOR v (Datum ConwayEra) instance FromCanonicalCBOR v (Datum ConwayEra)

deriving via LedgerCBOR v (BinaryData ConwayEra) instance ToCanonicalCBOR v (BinaryData ConwayEra)

deriving via
  LedgerCBOR v (BinaryData ConwayEra)
  instance
    FromCanonicalCBOR v (BinaryData ConwayEra)

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = UtxoKey
  type NamespaceEntry "utxo/v0" = UtxoOut

instance CanonicalCBOREntryEncoder "utxo/v0" UtxoOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"utxo/v0") n

instance CanonicalCBOREntryDecoder "utxo/v0" UtxoOut where
  decodeEntry = fromCanonicalCBOR
-}

instance IsCanonicalScript (AlonzoScript ConwayEra) where
  mkCanonicalScript (NativeScript n) = CanonicalScriptNative (mkCanonicalNativeScript n)
  mkCanonicalScript (PlutusScript bs) = CanonicalScriptPlutus (mkCanonicalPlutusScript bs)
  fromCanonicalScript (CanonicalScriptNative n) = NativeScript (fromCanonicalNativeScript n)
  fromCanonicalScript (CanonicalScriptPlutus n) = PlutusScript (fromCanonicalPlutusScript n)

instance IsCanonicalPlutusScript (PlutusScript ConwayEra) where
  mkCanonicalPlutusScript = undefined
  fromCanonicalPlutusScript = undefined

instance IsCanonicalNativeScript (Timelock ConwayEra) where
  mkCanonicalNativeScript = undefined
  fromCanonicalNativeScript = undefined

instance IsCanonicalDatum (Datum ConwayEra) where
  mkCanonicalDatum NoDatum = CanonicalNoDatum
  mkCanonicalDatum (DatumHash dh) = CanonicalDatumHash dh
  mkCanonicalDatum (Datum d) = CanonicalDatum (undefined d)
  fromCanonicalDatum CanonicalNoDatum = NoDatum
  fromCanonicalDatum (CanonicalDatumHash bs) = DatumHash bs
  fromCanonicalDatum (CanonicalDatum d) = Datum (undefined d)

instance IsCanonicalBabbageTxOut (Babbage.BabbageTxOut ConwayEra) where
  mkCanonicalBabbageTxOut = undefined
  fromCanonicalBabbageTxOut = undefined

instance IsCanonicalShelleyTxOut (Shelley.ShelleyTxOut ConwayEra) where
  mkCanonicalShelleyTxOut = undefined
  fromCanonicalShelleyTxOut = undefined