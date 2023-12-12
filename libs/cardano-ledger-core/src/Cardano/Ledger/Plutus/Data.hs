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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- This is needed for the `HeapWords (StrictMaybe (DataHash c))` instance
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Plutus.Data (
  PlutusData (..),
  Data (Data),
  unData,
  DataHash,
  upgradeData,
  hashData,
  getPlutusData,
  dataHashSize,
  BinaryData,
  hashBinaryData,
  makeBinaryData,
  binaryDataToData,
  dataToBinaryData,
  Datum (..),
  datumDataHash,
  translateDatum,
)
where

import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.HeapWords (HeapWords (..), heapWords0, heapWords1)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  DecoderError (..),
  EncCBOR (..),
  ToCBOR (..),
  decodeFullAnnotator,
  decodeNestedCborBytes,
  encodeTag,
  fromPlainDecoder,
  fromPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto (HASH))
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes (..),
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  mkMemoBytes,
  mkMemoized,
  shortToLazy,
 )
import Cardano.Ledger.SafeHash (
  HashAnnotated,
  SafeToHash (..),
  hashAnnotated,
 )
import qualified Codec.Serialise as Cborg (Serialise (..))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), Value (Null))
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Coerce (coerce)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1

-- ============================================================================
-- the newtype Data is a wrapper around the type that Plutus expects as data.
-- The newtype will memoize the serialized bytes.

-- | This is a wrapper with a phantom era for PV1.Data, since we need
-- something with kind (* -> *) for MemoBytes
newtype PlutusData era = PlutusData PV1.Data
  deriving newtype (Eq, Generic, Show, NFData, NoThunks, Cborg.Serialise)

instance Typeable era => EncCBOR (PlutusData era) where
  encCBOR (PlutusData d) = fromPlainEncoding $ Cborg.encode d

instance Typeable era => DecCBOR (Annotator (PlutusData era)) where
  decCBOR = pure <$> fromPlainDecoder Cborg.decode

newtype Data era = DataConstr (MemoBytes PlutusData era)
  deriving (Eq, Generic)
  deriving newtype (SafeToHash, ToCBOR, NFData)

-- | Encodes memoized bytes created upon construction.
instance Typeable era => EncCBOR (Data era)

instance Memoized Data where
  type RawType Data = PlutusData

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (Data era)

deriving via Mem PlutusData era instance Era era => DecCBOR (Annotator (Data era))

type instance MemoHashIndex PlutusData = EraIndependentData

instance EraCrypto era ~ c => HashAnnotated (Data era) EraIndependentData c where
  hashAnnotated = getMemoSafeHash

instance Typeable era => NoThunks (Data era)

pattern Data :: Era era => PV1.Data -> Data era
pattern Data p <- (getMemoRawType -> PlutusData p)
  where
    Data p = mkMemoized $ PlutusData p

{-# COMPLETE Data #-}

unData :: Data era -> PV1.Data
unData eraData =
  case getMemoRawType eraData of
    PlutusData plutusData -> plutusData

-- | Upgrade 'Data' from one era to another. While the underlying data will
-- remain the same, the memoised serialisation may change to reflect the
-- versioned serialisation of the new era.
upgradeData :: (Era era1, Era era2) => Data era1 -> Data era2
upgradeData (Data d) = Data d

getPlutusData :: Data era -> PV1.Data
getPlutusData (getMemoRawType -> PlutusData d) = d

-- | Inlined data must be stored in the most compact form because it contributes
-- to the memory overhead of the ledger state. Constructor is intentionally not
-- exported, in order to prevent invalid creation of data from arbitrary binary
-- data. Use `makeBinaryData` for smart construction.
newtype BinaryData era = BinaryData ShortByteString
  deriving newtype (Eq, NoThunks, Ord, Show, SafeToHash)
  deriving (Generic)

instance EraCrypto era ~ c => HashAnnotated (BinaryData era) EraIndependentData c

instance Typeable era => EncCBOR (BinaryData era) where
  encCBOR (BinaryData sbs) = encodeTag 24 <> encCBOR sbs

instance Era era => DecCBOR (BinaryData era) where
  decCBOR = do
    bs <- decodeNestedCborBytes
    either fail pure $! makeBinaryData (toShort bs)

-- | Construct `BinaryData` from a buffer of bytes, while ensuring that it can be later
-- safely converted to `Data` with `binaryDataToData`
makeBinaryData :: Era era => ShortByteString -> Either String (BinaryData era)
makeBinaryData sbs = do
  let binaryData = BinaryData sbs
  -- We need to verify that binary data is indeed valid Plutus Data.
  case decodeBinaryData binaryData of
    Left e -> Left $ "Invalid CBOR for Data: " <> show e
    Right _d -> Right binaryData

decodeBinaryData :: forall era. Era era => BinaryData era -> Either DecoderError (Data era)
decodeBinaryData (BinaryData sbs) = do
  plutusData <- decodeFullAnnotator (eraProtVerLow @era) "Data" decCBOR (fromStrict (fromShort sbs))
  pure (DataConstr (mkMemoBytes plutusData $ shortToLazy sbs))

-- | It is safe to convert `BinaryData` to `Data` because the only way to
-- construct `BinaryData` is through the smart constructor `makeBinaryData` that
-- takes care of validation.
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

instance Crypto c => HeapWords (StrictMaybe (DataHash c)) where
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
  deriving (Eq, Generic, NoThunks, Ord, Show)

instance Era era => EncCBOR (Datum era) where
  encCBOR d = encode $ case d of
    DatumHash dh -> Sum DatumHash 0 !> To dh
    Datum d' -> Sum Datum 1 !> To d'
    NoDatum -> OmitC NoDatum

instance Era era => DecCBOR (Datum era) where
  decCBOR = decode (Summands "Datum" decodeDatum)
    where
      decodeDatum 0 = SumD DatumHash <! From
      decodeDatum 1 = SumD Datum <! From
      decodeDatum k = Invalid k

instance Era era => ToJSON (Datum era) where
  toJSON d =
    case datumDataHash d of
      SNothing -> Null
      SJust dh -> toJSON dh
  toEncoding d =
    case datumDataHash d of
      SNothing -> toEncoding Null
      SJust dh -> toEncoding dh

-- | Get the Hash of the datum.
datumDataHash :: Era era => Datum era -> StrictMaybe (DataHash (EraCrypto era))
datumDataHash = \case
  NoDatum -> SNothing
  DatumHash dh -> SJust dh
  Datum bd -> SJust (hashBinaryData bd)

translateDatum ::
  EraCrypto era1 ~ EraCrypto era2 =>
  Datum era1 ->
  Datum era2
translateDatum = \case
  NoDatum -> NoDatum
  DatumHash dh -> DatumHash dh
  Datum bd -> Datum (coerce bd)
