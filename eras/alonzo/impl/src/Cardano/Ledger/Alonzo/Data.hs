{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- This is needed to make Plutus.Data instances
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Alonzo.Data
  ( Data (Data),
    DataHash,
    hashData,
    getPlutusData,
    dataHashSize,
    BinaryData,
    hashBinaryData,
    makeBinaryData,
    binaryDataToData,
    dataToBinaryData,
    decodeBinaryData,
    Datum (..),
    datumDataHash,

    -- * AlonzoTxAuxData
    AlonzoTxAuxData
      ( AlonzoTxAuxData,
        AlonzoTxAuxData',
        atadMetadata,
        atadTimelock,
        atadPlutus,
        atadMetadata',
        atadTimelock',
        atadPlutus'
      ),
    mkAlonzoTxAuxData,
    AuxiliaryDataHash (..),
    hashAlonzoTxAuxData,
    validateAlonzoTxAuxData,
    getAlonzoTxAuxDataScripts,

    -- * Deprecated
    AuxiliaryData,
  )
where

import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.HeapWords (HeapWords (..), heapWords0, heapWords1)
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), BinaryPlutus (..), validScript)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (ProtVer, StrictMaybe (..))
import Cardano.Ledger.Binary
  ( Annotator (..),
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    TokenType (..),
    decodeFullAnnotator,
    decodeNestedCborBytes,
    decodeStrictSeq,
    encodeTag,
    fromPlainDecoder,
    fromPlainEncoding,
    peekTokenType,
  )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.MemoBytes
  ( Mem,
    MemoBytes (..),
    MemoHashIndex,
    Memoized (RawType),
    getMemoRawType,
    getMemoSafeHash,
    mkMemoBytes,
    mkMemoized,
    shortToLazy,
  )
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeToHash (..),
    hashAnnotated,
  )
import Cardano.Ledger.Shelley.Metadata (Metadatum, validMetadatum)
import Cardano.Ledger.ShelleyMA.Timelocks
import qualified Codec.Serialise as Cborg (Serialise (..))
import Control.DeepSeq (NFData, deepseq)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import qualified PlutusLedgerApi.V1 as Plutus

-- =====================================================================
-- Plutus.Data is the type that Plutus expects as data.
-- It is imported from the Plutus package, but it needs a few additional
-- instances to also work in the ledger.

-- TODO: Move to PlutusCore.Data module
deriving instance NoThunks Plutus.Data

-- ============================================================================
-- the newtype Data is a wrapper around the type that Plutus expects as data.
-- The newtype will memoize the serialized bytes.

-- | This is a wrapper with a phantom era for Plutus.Data, since we need
-- something with kind (* -> *) for MemoBytes
newtype PlutusData era = PlutusData Plutus.Data
  deriving newtype (Eq, Generic, Show, NFData, NoThunks, Cborg.Serialise)

instance Typeable era => ToCBOR (PlutusData era) where
  toCBOR (PlutusData d) = fromPlainEncoding $ Cborg.encode d

instance Typeable era => FromCBOR (Annotator (PlutusData era)) where
  fromCBOR = pure <$> fromPlainDecoder Cborg.decode

newtype Data era = DataConstr (MemoBytes PlutusData era)
  deriving (Eq, Generic)
  deriving newtype (SafeToHash, ToCBOR, NFData)

instance Memoized Data where
  type RawType Data = PlutusData

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (Data era)

deriving via Mem PlutusData era instance Era era => FromCBOR (Annotator (Data era))

type instance MemoHashIndex PlutusData = EraIndependentData

instance (EraCrypto era ~ c) => HashAnnotated (Data era) EraIndependentData c where
  hashAnnotated = getMemoSafeHash

instance Typeable era => NoThunks (Data era)

pattern Data :: Era era => Plutus.Data -> Data era
pattern Data p <- (getMemoRawType -> PlutusData p)
  where
    Data p = mkMemoized $ PlutusData p

{-# COMPLETE Data #-}

getPlutusData :: Data era -> Plutus.Data
getPlutusData (getMemoRawType -> PlutusData d) = d

-- | Inlined data must be stored in the most compact form because it contributes
-- to the memory overhead of the ledger state. Constructor is intentionally not
-- exported, in order to prevent invalid creation of data from arbitrary binary
-- data. Use `makeBinaryData` for smart construction.
newtype BinaryData era = BinaryData ShortByteString
  deriving newtype (Eq, Ord, Show, SafeToHash)

instance (EraCrypto era ~ c) => HashAnnotated (BinaryData era) EraIndependentData c

instance Typeable era => ToCBOR (BinaryData era) where
  toCBOR (BinaryData sbs) = encodeTag 24 <> toCBOR sbs

instance Era era => FromCBOR (BinaryData era) where
  fromCBOR = do
    bs <- decodeNestedCborBytes
    either fail pure $! makeBinaryData (toShort bs)

makeBinaryData :: Era era => ShortByteString -> Either String (BinaryData era)
makeBinaryData sbs = do
  let binaryData = BinaryData sbs
  -- We need to verify that binary data is indeed valid Plutus Data.
  case decodeBinaryData binaryData of
    Left e -> Left $ "Invalid CBOR for Data: " <> show e
    Right _d -> Right binaryData

decodeBinaryData :: forall era. Era era => BinaryData era -> Either DecoderError (Data era)
decodeBinaryData (BinaryData sbs) = do
  plutusData <- decodeFullAnnotator (eraProtVerLow @era) "Data" fromCBOR (fromStrict (fromShort sbs))
  pure (DataConstr (mkMemoBytes plutusData $ shortToLazy sbs))

-- | It is safe to convert `BinaryData` to `Data` because the only way to
-- construct `BinaryData` is thorugh smart constructor `makeBinaryData` that
-- takes care of verification.
binaryDataToData :: Era era => BinaryData era -> Data era
binaryDataToData binaryData =
  case decodeBinaryData binaryData of
    Left errMsg ->
      error $ "Impossible: incorrectly encoded data: " ++ show errMsg
    Right d -> d

dataToBinaryData :: Era era => Data era -> BinaryData era
dataToBinaryData (DataConstr (Memo _ sbs)) = BinaryData sbs

hashBinaryData :: Era era => BinaryData era -> DataHash (EraCrypto era)
hashBinaryData = hashAnnotated

-- =============================================================================

hashData :: Era era => Data era -> DataHash (EraCrypto era)
hashData = hashAnnotated

-- Size of the datum hash attached to the output (could be Nothing)
dataHashSize :: StrictMaybe (DataHash c) -> Integer
dataHashSize SNothing = 0
dataHashSize (SJust _) = 10

instance (CC.Crypto c) => HeapWords (StrictMaybe (DataHash c)) where
  heapWords SNothing = heapWords0
  heapWords (SJust a) = heapWords1 a

-- ============================================================================
-- Datum

-- | Datum can be described by a either a data hash or binary data, but not
-- both. It can also be neither one of them.
data Datum era
  = NoDatum
  | DatumHash !(DataHash (EraCrypto era))
  | Datum !(BinaryData era)
  deriving (Eq, Ord, Show)

instance Era era => ToCBOR (Datum era) where
  toCBOR d = encode $ case d of
    DatumHash dh -> Sum DatumHash 0 !> To dh
    Datum d' -> Sum Datum 1 !> To d'
    NoDatum -> OmitC NoDatum

instance Era era => FromCBOR (Datum era) where
  fromCBOR = decode (Summands "Datum" decodeDatum)
    where
      decodeDatum 0 = SumD DatumHash <! From
      decodeDatum 1 = SumD Datum <! From
      decodeDatum k = Invalid k

-- | Get the Hash of the datum.
datumDataHash :: Era era => Datum era -> StrictMaybe (DataHash (EraCrypto era))
datumDataHash = \case
  NoDatum -> SNothing
  DatumHash dh -> SJust dh
  Datum bd -> SJust (hashBinaryData bd)

-- =============================================================================
-- Version without serialized bytes

data AlonzoTxAuxDataRaw era = AlonzoTxAuxDataRaw
  { atadrMetadata :: !(Map Word64 Metadatum),
    atadrTimelock :: !(StrictSeq (Timelock era)),
    atadrPlutus :: !(Map Language (NE.NonEmpty BinaryPlutus))
  }
  deriving (Generic)

deriving instance Eq (Timelock era) => Eq (AlonzoTxAuxDataRaw era)

deriving instance Show (Timelock era) => Show (AlonzoTxAuxDataRaw era)

instance NFData (Timelock era) => NFData (AlonzoTxAuxDataRaw era)

deriving via
  InspectHeapNamed "AlonzoTxAuxDataRaw" (AlonzoTxAuxDataRaw era)
  instance
    NoThunks (AlonzoTxAuxDataRaw era)

instance Era era => ToCBOR (AlonzoTxAuxDataRaw era) where
  toCBOR AlonzoTxAuxDataRaw {atadrMetadata, atadrTimelock, atadrPlutus} =
    encode $
      Tag 259 $
        Keyed
          ( \m ts mps1 mps2 ->
              AlonzoTxAuxDataRaw m ts $
                Map.fromList [(pv, ps) | (pv, Just ps) <- [(PlutusV1, mps1), (PlutusV2, mps2)]]
          )
          !> Omit null (Key 0 $ To atadrMetadata)
          !> Omit null (Key 1 $ To atadrTimelock)
          !> Omit isNothing (Key 2 $ E (maybe mempty toCBOR) (Map.lookup PlutusV1 atadrPlutus))
          !> Omit isNothing (Key 3 $ E (maybe mempty toCBOR) (Map.lookup PlutusV2 atadrPlutus))

-- | Helper function that will construct Auxiliary data from Metadatum map and a list of scripts.
--
-- Note that the relative order of same type scripts will be preserved.
mkAlonzoTxAuxData ::
  forall f era.
  (Foldable f, Era era) =>
  Map Word64 Metadatum ->
  f (AlonzoScript era) ->
  AlonzoTxAuxData era
mkAlonzoTxAuxData atadrMetadata allScripts =
  mkMemoized $ AlonzoTxAuxDataRaw {atadrMetadata, atadrTimelock, atadrPlutus}
  where
    partitionScripts (tss, pss1, pss2) =
      \case
        TimelockScript ts -> (ts :<| tss, pss1, pss2)
        PlutusScript PlutusV1 ps1 -> (tss, BinaryPlutus ps1 : pss1, pss2)
        PlutusScript PlutusV2 ps2 -> (tss, pss1, BinaryPlutus ps2 : pss2)
    (atadrTimelock, plutusV1Scripts, plutusV2Scripts) =
      foldr (flip partitionScripts) (mempty, mempty, mempty) allScripts
    atadrPlutus =
      Map.fromList
        [ (lang, scripts)
          | (lang, Just scripts) <-
              [ (PlutusV1, NE.nonEmpty plutusV1Scripts),
                (PlutusV2, NE.nonEmpty plutusV2Scripts)
              ]
        ]

getAlonzoTxAuxDataScripts :: Era era => AlonzoTxAuxData era -> StrictSeq (AlonzoScript era)
getAlonzoTxAuxDataScripts AlonzoTxAuxData' {atadTimelock' = timelocks, atadPlutus' = plutus} =
  mconcat $
    (TimelockScript <$> timelocks)
      : [ PlutusScript lang . unBinaryPlutus <$> StrictSeq.fromList (NE.toList plutusScripts)
          | lang <- [PlutusV1 ..],
            Just plutusScripts <- [Map.lookup lang plutus]
        ]

instance Era era => FromCBOR (Annotator (AlonzoTxAuxDataRaw era)) where
  fromCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeShelley
      TypeMapLen64 -> decodeShelley
      TypeMapLenIndef -> decodeShelley
      TypeListLen -> decodeShelleyMA
      TypeListLen64 -> decodeShelleyMA
      TypeListLenIndef -> decodeShelleyMA
      TypeTag -> decodeAlonzo
      TypeTag64 -> decodeAlonzo
      _ -> error "Failed to decode AuxiliaryData"
    where
      decodeShelley =
        decode
          ( Ann (Emit AlonzoTxAuxDataRaw)
              <*! Ann From
              <*! Ann (Emit StrictSeq.empty)
              <*! Ann (Emit Map.empty)
          )
      decodeShelleyMA =
        decode
          ( Ann (RecD AlonzoTxAuxDataRaw)
              <*! Ann From
              <*! D
                (sequence <$> decodeStrictSeq fromCBOR)
              <*! Ann (Emit Map.empty)
          )
      decodeAlonzo =
        decode $
          TagD 259 $
            SparseKeyed "AuxiliaryData" (pure emptyAuxData) auxDataField []

      addPlutusScripts lang scripts ad =
        case NE.nonEmpty scripts of
          Nothing -> ad
          Just neScripts ->
            -- Avoid leaks by , since non empty list is lazy.
            neScripts `deepseq` ad {atadrPlutus = Map.insert lang neScripts $ atadrPlutus ad}

      auxDataField :: Word -> Field (Annotator (AlonzoTxAuxDataRaw era))
      auxDataField 0 = fieldA (\x ad -> ad {atadrMetadata = x}) From
      auxDataField 1 =
        fieldAA
          (\x ad -> ad {atadrTimelock = atadrTimelock ad <> x})
          (D (sequence <$> decodeStrictSeq fromCBOR))
      auxDataField 2 = fieldA (addPlutusScripts PlutusV1) From
      auxDataField 3 = fieldA (addPlutusScripts PlutusV2) From
      auxDataField n = field (\_ t -> t) (Invalid n)

emptyAuxData :: AlonzoTxAuxDataRaw era
emptyAuxData = AlonzoTxAuxDataRaw mempty mempty mempty

-- ================================================================================
-- Version with serialized bytes.

newtype AlonzoTxAuxData era = AuxiliaryDataConstr (MemoBytes AlonzoTxAuxDataRaw era)
  deriving newtype (ToCBOR, SafeToHash)

instance Memoized AlonzoTxAuxData where
  type RawType AlonzoTxAuxData = AlonzoTxAuxDataRaw

type AuxiliaryData era = AlonzoTxAuxData era

{-# DEPRECATED AuxiliaryData "Use `AlonzoTxAuxData` instead" #-}

instance CC.Crypto c => EraTxAuxData (AlonzoEra c) where
  type TxAuxData (AlonzoEra c) = AlonzoTxAuxData (AlonzoEra c)
  hashTxAuxData = hashAlonzoTxAuxData
  validateTxAuxData = validateAlonzoTxAuxData

hashAlonzoTxAuxData ::
  (HashAlgorithm (CC.HASH c), HashAnnotated x EraIndependentTxAuxData c) =>
  x ->
  AuxiliaryDataHash c
hashAlonzoTxAuxData x = AuxiliaryDataHash (hashAnnotated x)

validateAlonzoTxAuxData ::
  Era era =>
  ProtVer ->
  AuxiliaryData era ->
  Bool
validateAlonzoTxAuxData pv auxData@AlonzoTxAuxData {atadMetadata = metadata} =
  all validMetadatum metadata
    && all (validScript pv) (getAlonzoTxAuxDataScripts auxData)

instance (EraCrypto era ~ c) => HashAnnotated (AuxiliaryData era) EraIndependentTxAuxData c where
  hashAnnotated = getMemoSafeHash

deriving newtype instance NFData (AuxiliaryData era)

deriving instance Eq (AuxiliaryData era)

deriving instance (HashAlgorithm (HASH (EraCrypto era))) => Show (AuxiliaryData era)

type instance MemoHashIndex AlonzoTxAuxDataRaw = EraIndependentTxAuxData

deriving via
  InspectHeapNamed "AlonzoTxAuxDataRaw" (AuxiliaryData era)
  instance
    NoThunks (AuxiliaryData era)

deriving via
  (Mem AlonzoTxAuxDataRaw era)
  instance
    Era era => FromCBOR (Annotator (AuxiliaryData era))

pattern AlonzoTxAuxData ::
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Timelock era) ->
  Map Language (NE.NonEmpty BinaryPlutus) ->
  AuxiliaryData era
pattern AlonzoTxAuxData {atadMetadata, atadTimelock, atadPlutus} <-
  (getMemoRawType -> AlonzoTxAuxDataRaw atadMetadata atadTimelock atadPlutus)
  where
    AlonzoTxAuxData atadrMetadata atadrTimelock atadrPlutus =
      mkMemoized $ AlonzoTxAuxDataRaw {atadrMetadata, atadrTimelock, atadrPlutus}

{-# COMPLETE AlonzoTxAuxData #-}

pattern AlonzoTxAuxData' ::
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Timelock era) ->
  Map Language (NE.NonEmpty BinaryPlutus) ->
  AlonzoTxAuxData era
pattern AlonzoTxAuxData' {atadMetadata', atadTimelock', atadPlutus'} <-
  (getMemoRawType -> AlonzoTxAuxDataRaw atadMetadata' atadTimelock' atadPlutus')

{-# COMPLETE AlonzoTxAuxData' #-}
