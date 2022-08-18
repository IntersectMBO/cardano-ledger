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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
    -- $
    AlonzoAuxiliaryData (AlonzoAuxiliaryData, AlonzoAuxiliaryData', scripts, txMD),
    AuxiliaryDataHash (..),
    hashAlonzoAuxiliaryData,
    validateAlonzoAuxiliaryData,

    -- * Deprecated
    AuxiliaryData,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    TokenType (..),
    decodeAnnotator,
    decodeNestedCborBytes,
    encodeTag,
    peekTokenType,
    withSlice,
  )
import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), validScript)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (ProtVer, StrictMaybe (..))
import Cardano.Ledger.Core hiding (AuxiliaryData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), MemoHashIndex, memoBytes, mkMemoBytes, shortToLazy)
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeToHash (..),
    hashAnnotated,
  )
import Cardano.Ledger.Serialization (mapFromCBOR)
import Cardano.Ledger.Shelley.Metadata (Metadatum, validMetadatum)
import Cardano.Prelude (HeapWords (..), heapWords0, heapWords1)
import qualified Codec.Serialise as Cborg (Serialise (..))
import Control.DeepSeq (NFData)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Coders
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import qualified Plutus.V1.Ledger.Api as Plutus

-- =====================================================================
-- Plutus.Data is the type that Plutus expects as data.
-- It is imported from the Plutus package, but it needs a few additional
-- instances to also work in the ledger.

instance FromCBOR (Annotator Plutus.Data) where
  fromCBOR = pure <$> Cborg.decode

instance ToCBOR Plutus.Data where
  toCBOR = Cborg.encode

deriving instance NoThunks Plutus.Data

-- ============================================================================
-- the newtype Data is a wrapper around the type that Plutus expects as data.
-- The newtype will memoize the serialized bytes.

-- | This is a wrapper with a phantom era for Plutus.Data, since we need
-- something with kind (* -> *) for MemoBytes
newtype PlutusData era = PlutusData Plutus.Data
  deriving newtype (Eq, Generic, Show, ToCBOR, NFData, NoThunks, Cborg.Serialise)

instance Typeable era => FromCBOR (Annotator (PlutusData era)) where
  fromCBOR = pure <$> Cborg.decode

newtype Data era = DataConstr (MemoBytes PlutusData era)
  deriving (Eq, Generic)
  deriving newtype (SafeToHash, ToCBOR, NFData)

deriving instance HashAlgorithm (HASH (Crypto era)) => Show (Data era)

instance (Typeable era, Era era) => FromCBOR (Annotator (Data era)) where
  fromCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice fromCBOR
    pure (Annotator (\fullbytes -> DataConstr (mkMemoBytes (getT fullbytes) (getBytes fullbytes))))

type instance MemoHashIndex PlutusData = EraIndependentData

instance (Crypto era ~ c) => HashAnnotated (Data era) EraIndependentData c where
  hashAnnotated (DataConstr mb) = mbHash mb

instance Typeable era => NoThunks (Data era)

pattern Data :: Era era => Plutus.Data -> Data era
pattern Data p <-
  DataConstr (Memo (PlutusData p) _)
  where
    Data p = DataConstr $ memoBytes (To $ PlutusData p)

{-# COMPLETE Data #-}

getPlutusData :: Era era => Data era -> Plutus.Data
getPlutusData (DataConstr (Memo (PlutusData d) _)) = d

-- | Inlined data must be stored in the most compact form because it contributes
-- to the memory overhead of the ledger state. Constructor is intentionally not
-- exported, in order to prevent invalid creation of data from arbitrary binary
-- data. Use `makeBinaryData` for smart construction.
newtype BinaryData era = BinaryData ShortByteString
  deriving newtype (Eq, Ord, Show, SafeToHash)

instance (Crypto era ~ c) => HashAnnotated (BinaryData era) EraIndependentData c

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

decodeBinaryData :: Era era => BinaryData era -> Either DecoderError (Data era)
decodeBinaryData (BinaryData sbs) = do
  plutusData <- decodeAnnotator "Data" fromCBOR (fromStrict (fromShort sbs))
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

hashBinaryData :: Era era => BinaryData era -> DataHash (Crypto era)
hashBinaryData = hashAnnotated

-- =============================================================================

hashData :: Era era => Data era -> DataHash (Crypto era)
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
  | DatumHash !(DataHash (Crypto era))
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
datumDataHash :: Era era => Datum era -> StrictMaybe (DataHash (Crypto era))
datumDataHash = \case
  NoDatum -> SNothing
  DatumHash dh -> SJust dh
  Datum bd -> SJust (hashBinaryData bd)

-- =============================================================================
-- Version without serialized bytes

data AuxiliaryDataRaw era = AuxiliaryDataRaw
  { txMD' :: !(Map Word64 Metadatum),
    scripts' :: !(StrictSeq (Script era))
  }
  deriving (Generic)

deriving instance Eq (Script era) => Eq (AuxiliaryDataRaw era)

deriving instance Show (Script era) => Show (AuxiliaryDataRaw era)

instance NFData (Script era) => NFData (AuxiliaryDataRaw era)

deriving via
  InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryDataRaw era)
  instance
    NoThunks (AuxiliaryDataRaw era)

instance
  ( Typeable era,
    Script era ~ AlonzoScript era,
    ToCBOR (Script era),
    Typeable (Crypto era)
  ) =>
  ToCBOR (AuxiliaryDataRaw era)
  where
  toCBOR (AuxiliaryDataRaw m s) =
    encode (encodeRaw m s)

encodeRaw ::
  ( Script era ~ AlonzoScript era,
    Typeable era
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  Encode ('Closed 'Sparse) (AuxiliaryDataRaw era)
encodeRaw metadata allScripts =
  Tag 259 $
    Keyed
      (\m tss p1 p2 -> AuxiliaryDataRaw m (StrictSeq.fromList $ tss <> p1 <> p2))
      !> Omit null (Key 0 $ mapEncode metadata)
      !> Omit null (Key 1 $ E (encodeFoldable . mapMaybe getTimelock) timelocks)
      !> Omit null (Key 2 $ E (encodeFoldable . mapMaybe getPlutus) plutusV1Scripts)
      !> Omit null (Key 3 $ E (encodeFoldable . mapMaybe getPlutus) plutusV2Scripts)
  where
    getTimelock (TimelockScript x) = Just x
    getTimelock _ = Nothing
    getPlutus (PlutusScript _ x) = Just x
    getPlutus _ = Nothing
    sortScripts (ts, v1, v2) s@(TimelockScript _) = (s : ts, v1, v2)
    sortScripts (ts, v1, v2) s@(PlutusScript PlutusV1 _) = (ts, s : v1, v2)
    sortScripts (ts, v1, v2) s@(PlutusScript PlutusV2 _) = (ts, v1, s : v2)
    (timelocks, plutusV1Scripts, plutusV2Scripts) =
      foldl' sortScripts (mempty, mempty, mempty) allScripts

instance
  ( Era era,
    FromCBOR (Annotator (Script era)),
    Script era ~ AlonzoScript era
  ) =>
  FromCBOR (Annotator (AuxiliaryDataRaw era))
  where
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
          ( Ann (Emit AuxiliaryDataRaw)
              <*! Ann (D mapFromCBOR)
              <*! Ann (Emit StrictSeq.empty)
          )
      decodeShelleyMA =
        decode
          ( Ann (RecD AuxiliaryDataRaw)
              <*! Ann (D mapFromCBOR)
              <*! D
                ( sequence
                    <$> decodeStrictSeq
                      (fmap TimelockScript <$> fromCBOR)
                )
          )
      decodeAlonzo =
        decode $
          TagD 259 $
            SparseKeyed "AuxiliaryData" (pure emptyAuxData) auxDataField []

      auxDataField :: Word -> Field (Annotator (AuxiliaryDataRaw era))
      auxDataField 0 = fieldA (\x ad -> ad {txMD' = x}) (D mapFromCBOR)
      auxDataField 1 =
        fieldAA
          (\x ad -> ad {scripts' = scripts' ad <> (TimelockScript <$> x)})
          (D (sequence <$> decodeStrictSeq fromCBOR))
      auxDataField 2 =
        fieldA
          (\x ad -> ad {scripts' = scripts' ad <> (PlutusScript PlutusV1 <$> x)})
          (D (decodeStrictSeq fromCBOR))
      auxDataField 3 =
        fieldA
          (\x ad -> ad {scripts' = scripts' ad <> (PlutusScript PlutusV2 <$> x)})
          (D (decodeStrictSeq fromCBOR))
      auxDataField n = field (\_ t -> t) (Invalid n)

emptyAuxData :: AuxiliaryDataRaw era
emptyAuxData = AuxiliaryDataRaw mempty mempty

-- ================================================================================
-- Version with serialized bytes.

newtype AlonzoAuxiliaryData era = AuxiliaryDataConstr (MemoBytes AuxiliaryDataRaw era)
  deriving newtype (ToCBOR, SafeToHash)

type AuxiliaryData era = AlonzoAuxiliaryData era

{-# DEPRECATED AuxiliaryData "Use `AlonzoAuxiliaryData` instead" #-}

instance CC.Crypto c => EraAuxiliaryData (AlonzoEra c) where
  type AuxiliaryData (AlonzoEra c) = AlonzoAuxiliaryData (AlonzoEra c)
  hashAuxiliaryData = hashAlonzoAuxiliaryData
  validateAuxiliaryData = validateAlonzoAuxiliaryData

hashAlonzoAuxiliaryData ::
  (HashAlgorithm (CC.HASH crypto), HashAnnotated x EraIndependentAuxiliaryData crypto) =>
  x ->
  AuxiliaryDataHash crypto
hashAlonzoAuxiliaryData x = AuxiliaryDataHash (hashAnnotated x)

validateAlonzoAuxiliaryData ::
  (Era era, ToCBOR (Script era), Script era ~ AlonzoScript era) =>
  ProtVer ->
  AuxiliaryData era ->
  Bool
validateAlonzoAuxiliaryData pv (AlonzoAuxiliaryData metadata scrips) =
  all validMetadatum metadata
    && all (validScript pv) scrips

instance (Crypto era ~ c) => HashAnnotated (AuxiliaryData era) EraIndependentAuxiliaryData c where
  hashAnnotated (AuxiliaryDataConstr mb) = mbHash mb

deriving newtype instance NFData (Script era) => NFData (AuxiliaryData era)

deriving instance Eq (AuxiliaryData era)

deriving instance (Show (Script era), HashAlgorithm (HASH (Crypto era))) => Show (AuxiliaryData era)

type instance MemoHashIndex AuxiliaryDataRaw = EraIndependentAuxiliaryData

deriving via InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryData era) instance NoThunks (AuxiliaryData era)

deriving via
  (Mem AuxiliaryDataRaw era)
  instance
    ( Era era,
      FromCBOR (Annotator (Script era)),
      AlonzoScript era ~ Script era -- FIXME: this smells fishy
    ) =>
    FromCBOR (Annotator (AuxiliaryData era))

pattern AlonzoAuxiliaryData ::
  ( Era era,
    ToCBOR (Script era),
    Script era ~ AlonzoScript era
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  AuxiliaryData era
pattern AlonzoAuxiliaryData {txMD, scripts} <-
  AuxiliaryDataConstr (Memo (AuxiliaryDataRaw txMD scripts) _)
  where
    AlonzoAuxiliaryData m s =
      AuxiliaryDataConstr
        ( memoBytes
            (encodeRaw m s)
        )

{-# COMPLETE AlonzoAuxiliaryData #-}

pattern AlonzoAuxiliaryData' ::
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  AuxiliaryData era
pattern AlonzoAuxiliaryData' txMD_ scripts_ <-
  AuxiliaryDataConstr (Memo (AuxiliaryDataRaw txMD_ scripts_) _)

{-# COMPLETE AlonzoAuxiliaryData' #-}
