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
    -- $
    AuxiliaryData (AuxiliaryData, AuxiliaryData', scripts, txMD),
    AuxiliaryDataHash (..),
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
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (Script (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (DataHash, EraIndependentAuxiliaryData, EraIndependentData)
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeToHash (..),
    hashAnnotated,
  )
import Cardano.Ledger.Serialization (mapFromCBOR)
import Cardano.Ledger.Shelley.Metadata (Metadatum)
import Cardano.Prelude (HeapWords (..), heapWords0, heapWords1)
import qualified Codec.Serialise as Cborg (Serialise (..))
import Control.DeepSeq (NFData)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Coders
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
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

newtype Data era = DataConstr (MemoBytes Plutus.Data)
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (SafeToHash, ToCBOR, NFData)

instance Typeable era => FromCBOR (Annotator (Data era)) where
  fromCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice fromCBOR
    pure (Annotator (\fullbytes -> DataConstr (Memo (getT fullbytes) (toShort (toStrict (getBytes fullbytes))))))

instance (Crypto era ~ c) => HashAnnotated (Data era) EraIndependentData c

instance NoThunks (Data era)

pattern Data :: Plutus.Data -> Data era
pattern Data p <-
  DataConstr (Memo p _)
  where
    Data p = DataConstr (memoBytes (To p))

{-# COMPLETE Data #-}

getPlutusData :: Data era -> Plutus.Data
getPlutusData (DataConstr (Memo d _)) = d

-- | Inlined data must be stored in the most compact form because it contributes
-- to the memory overhead of the ledger state. Constructor is intentionally not
-- exported, in order to prevent invalid creation of data from arbitrary binary
-- data. Use `makeBinaryData` for smart construction.
newtype BinaryData era = BinaryData ShortByteString
  deriving newtype (Eq, Ord, Show, SafeToHash)

instance (Crypto era ~ c) => HashAnnotated (BinaryData era) EraIndependentData c

instance Typeable era => ToCBOR (BinaryData era) where
  toCBOR (BinaryData sbs) = encodeTag 24 <> toCBOR sbs

instance Typeable era => FromCBOR (BinaryData era) where
  fromCBOR = do
    bs <- decodeNestedCborBytes
    either fail pure $! makeBinaryData (toShort bs)

makeBinaryData :: ShortByteString -> Either String (BinaryData era)
makeBinaryData sbs = do
  let binaryData = BinaryData sbs
  -- We need to verify that binary data is indeed valid Plutus Data.
  case decodeBinaryData binaryData of
    Left e -> Left $ "Invalid CBOR for Data: " <> show e
    Right _d -> Right binaryData

decodeBinaryData :: BinaryData era -> Either DecoderError (Data era)
decodeBinaryData (BinaryData sbs) = do
  plutusData <- decodeAnnotator "Data" fromCBOR (fromStrict (fromShort sbs))
  pure (DataConstr (Memo plutusData sbs))

-- | It is safe to convert `BinaryData` to `Data` because the only way to
-- construct `BinaryData` is thorugh smart constructor `makeBinaryData` that
-- takes care of verification.
binaryDataToData :: BinaryData era -> Data era
binaryDataToData binaryData =
  case decodeBinaryData binaryData of
    Left errMsg ->
      error $ "Impossible: incorrectly encoded data: " ++ show errMsg
    Right d -> d

dataToBinaryData :: Data era -> BinaryData era
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

-- =============================================================================
-- Version without serialized bytes

data AuxiliaryDataRaw era = AuxiliaryDataRaw
  { txMD' :: !(Map Word64 Metadatum),
    scripts' :: !(StrictSeq (Core.Script era))
  }
  deriving (Generic)

deriving instance Eq (Core.Script era) => Eq (AuxiliaryDataRaw era)

deriving instance Show (Core.Script era) => Show (AuxiliaryDataRaw era)

instance NFData (Core.Script era) => NFData (AuxiliaryDataRaw era)

deriving via
  InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryDataRaw era)
  instance
    NoThunks (AuxiliaryDataRaw era)

instance
  ( Typeable era,
    Ord (Core.Script era),
    Core.Script era ~ Script era,
    ToCBOR (Core.Script era),
    Typeable (Crypto era)
  ) =>
  ToCBOR (AuxiliaryDataRaw era)
  where
  toCBOR (AuxiliaryDataRaw m s) =
    encode (encodeRaw m s)

encodeRaw ::
  ( Core.Script era ~ Script era,
    Typeable (Crypto era)
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Core.Script era) ->
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
    Ord (Core.Script era),
    FromCBOR (Annotator (Core.Script era)),
    Core.Script era ~ Script era
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

newtype AuxiliaryData era = AuxiliaryDataConstr (MemoBytes (AuxiliaryDataRaw era))
  deriving newtype (ToCBOR, SafeToHash)

instance (Crypto era ~ c) => HashAnnotated (AuxiliaryData era) EraIndependentAuxiliaryData c

deriving newtype instance NFData (Core.Script era) => NFData (AuxiliaryData era)

deriving instance Eq (AuxiliaryData era)

deriving instance Show (Core.Script era) => Show (AuxiliaryData era)

deriving via InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryData era) instance NoThunks (AuxiliaryData era)

deriving via
  (Mem (AuxiliaryDataRaw era))
  instance
    ( Era era,
      Ord (Core.Script era),
      FromCBOR (Annotator (Core.Script era)),
      Script era ~ Core.Script era -- FIXME: this smells fishy
    ) =>
    FromCBOR (Annotator (AuxiliaryData era))

pattern AuxiliaryData ::
  ( Era era,
    ToCBOR (Core.Script era),
    Core.Script era ~ Script era,
    Ord (Core.Script era)
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Core.Script era) ->
  AuxiliaryData era
pattern AuxiliaryData {txMD, scripts} <-
  AuxiliaryDataConstr (Memo (AuxiliaryDataRaw txMD scripts) _)
  where
    AuxiliaryData m s =
      AuxiliaryDataConstr
        ( memoBytes
            (encodeRaw m s)
        )

{-# COMPLETE AuxiliaryData #-}

pattern AuxiliaryData' ::
  Map Word64 Metadatum ->
  StrictSeq (Core.Script era) ->
  AuxiliaryData era
pattern AuxiliaryData' txMD_ scripts_ <-
  AuxiliaryDataConstr (Memo (AuxiliaryDataRaw txMD_ scripts_) _)

{-# COMPLETE AuxiliaryData' #-}
