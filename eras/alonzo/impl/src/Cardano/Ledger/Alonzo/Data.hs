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
  ( Data (Data, ..),
    DataHash,
    hashData,
    getPlutusData,
    dataHashSize,
    -- $
    AuxiliaryData (AuxiliaryData, scripts, txMD),
    AuxiliaryDataHash (..),
    -- $
    ppPlutusData,
    ppData,
    ppAuxiliaryData,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), TokenType (..), peekTokenType, withSlice)
import Cardano.Ledger.Alonzo.Scripts
  ( Script (..),
    isPlutusScript,
  )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes
  ( EraIndependentAuxiliaryData,
    EraIndependentData,
  )
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppInteger,
    ppList,
    ppLong,
    ppMap,
    ppMetadatum,
    ppPair,
    ppSexp,
    ppStrictSeq,
    ppWord64,
  )
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash (..),
    hashAnnotated,
  )
import Cardano.Ledger.Serialization (mapFromCBOR)
import Cardano.Prelude (HeapWords (..), heapWords0, heapWords1)
import qualified Codec.Serialise as Cborg (Serialise (..))
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Coders
import qualified Data.Foldable as Foldable
import qualified Data.List as List
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
import Shelley.Spec.Ledger.Metadata (Metadatum)

-- =====================================================================
-- Plutus.Data is the type that Plutus expects as data.
-- It is imported from the Plutus package, but it needs a few additional
-- instances to also work in the ledger.

instance FromCBOR (Annotator Plutus.Data) where
  fromCBOR = pure <$> Cborg.decode

instance ToCBOR Plutus.Data where
  toCBOR = Cborg.encode

deriving anyclass instance NoThunks Plutus.BuiltinByteString

deriving instance NoThunks Plutus.Data

-- ============================================================================
-- the newtype Data is a wrapper around the type that Plutus expects as data.
-- The newtype will memoize the serialized bytes.

newtype Data era = DataConstr (MemoBytes Plutus.Data)
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (SafeToHash, ToCBOR)

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

getPlutusData :: Data era -> Plutus.Data
getPlutusData (DataConstr (Memo d _)) = d

-- =============================================================================

type DataHash crypto = SafeHash crypto EraIndependentData

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

deriving via InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryDataRaw era) instance NoThunks (AuxiliaryDataRaw era)

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
      (\m tss pss -> AuxiliaryDataRaw m (StrictSeq.fromList $ tss <> pss))
      !> Omit null (Key 0 $ mapEncode metadata)
      !> Omit null (Key 1 $ E (encodeFoldable . mapMaybe getTimelock) timelocks)
      !> Omit null (Key 2 $ E (encodeFoldable . mapMaybe getPlutus) plutusScripts)
  where
    getTimelock (TimelockScript x) = Just x
    getTimelock _ = Nothing
    getPlutus (PlutusScript x) = Just x
    getPlutus _ = Nothing
    (plutusScripts, timelocks) =
      List.partition
        isPlutusScript
        (Foldable.toList allScripts)

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
          (\x ad -> ad {scripts' = scripts' ad <> (PlutusScript <$> x)})
          (D (decodeStrictSeq fromCBOR))
      auxDataField n = field (\_ t -> t) (Invalid n)

emptyAuxData :: AuxiliaryDataRaw era
emptyAuxData = AuxiliaryDataRaw mempty mempty

-- ================================================================================
-- Version with serialized bytes.

newtype AuxiliaryData era = AuxiliaryDataConstr (MemoBytes (AuxiliaryDataRaw era))
  deriving newtype (ToCBOR, SafeToHash)

instance (Crypto era ~ c) => HashAnnotated (AuxiliaryData era) EraIndependentAuxiliaryData c

deriving instance Eq (Core.Script era) => Eq (AuxiliaryData era)

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

-- =======================================================

ppPlutusData :: Plutus.Data -> PDoc
ppPlutusData (Plutus.Constr tag args) = ppSexp "Constr" [ppInteger tag, ppList ppPlutusData args]
ppPlutusData (Plutus.Map pairs) = ppSexp "Map" [ppList (ppPair ppPlutusData ppPlutusData) pairs]
ppPlutusData (Plutus.List xs) = ppSexp "List" [ppList ppPlutusData xs]
ppPlutusData (Plutus.I i) = ppSexp "I" [ppInteger i]
ppPlutusData (Plutus.B bytes) = ppSexp "B" [ppLong bytes]

instance PrettyA Plutus.Data where prettyA = ppPlutusData

ppData :: Data era -> PDoc
ppData (DataConstr (Memo x _)) = ppSexp "Data" [ppPlutusData x]

instance PrettyA (Data era) where prettyA = ppData

ppAuxiliaryData :: (PrettyA (Core.Script era)) => AuxiliaryData era -> PDoc
ppAuxiliaryData (AuxiliaryDataConstr (Memo (AuxiliaryDataRaw m s) _)) =
  ppSexp "AuxiliaryData" [ppMap ppWord64 ppMetadatum m, ppStrictSeq prettyA s]

instance (PrettyA (Core.Script era)) => PrettyA (AuxiliaryData era) where prettyA = ppAuxiliaryData
