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
-- This is needed to make PlutusLedgerApi.V1.Data instances
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Alonzo.Scripts.Data (
  Data (Data),
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
)
where

import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.HeapWords (HeapWords (..), heapWords0, heapWords1)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (
  Annotator (..),
  DecoderError (..),
  EncCBOR (..),
  FromCBOR (..),
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
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1 -- NOTE PV1.Data === PV2.Data

-- =====================================================================
-- PV1.Data is the type that Plutus expects as data. For both V1 and V2.
-- It is imported from the Plutus package, but it needs a few additional
-- instances to also work in the ledger.
deriving instance NoThunks PV1.Data

-- ============================================================================
-- the newtype Data is a wrapper around the type that Plutus expects as data.
-- The newtype will memoize the serialized bytes.

-- | This is a wrapper with a phantom era for PV1.Data, since we need
-- something with kind (* -> *) for MemoBytes
newtype PlutusData era = PlutusData PV1.Data
  deriving newtype (Eq, Generic, Show, NFData, NoThunks, Cborg.Serialise)

instance Typeable era => ToCBOR (PlutusData era) where
  toCBOR (PlutusData d) = fromPlainEncoding $ Cborg.encode d

instance Typeable era => FromCBOR (Annotator (PlutusData era)) where
  fromCBOR = pure <$> fromPlainDecoder Cborg.decode

newtype Data era = DataConstr (MemoBytes PlutusData era)
  deriving (Eq, Generic)
  deriving newtype (SafeToHash, EncCBOR, NFData)

-- Data is used inside of other types, so it does need ToCBOR, which it gets by
-- piggybacking on the EncCBOR
instance Typeable era => ToCBOR (Data era)

instance Memoized Data where
  type RawType Data = PlutusData

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (Data era)

deriving via Mem PlutusData era instance Era era => FromCBOR (Annotator (Data era))

type instance MemoHashIndex PlutusData = EraIndependentData

instance (EraCrypto era ~ c) => HashAnnotated (Data era) EraIndependentData c where
  hashAnnotated = getMemoSafeHash

instance Typeable era => NoThunks (Data era)

pattern Data :: Era era => PV1.Data -> Data era
pattern Data p <- (getMemoRawType -> PlutusData p)
  where
    Data p = mkMemoized $ PlutusData p

{-# COMPLETE Data #-}

getPlutusData :: Data era -> PV1.Data
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

instance (Crypto c) => HeapWords (StrictMaybe (DataHash c)) where
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
