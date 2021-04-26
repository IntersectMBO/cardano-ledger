{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
    AuxiliaryData (AuxiliaryData, scripts, dats, txMD),
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
    ppSet,
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
import Cardano.Prelude (HeapWords (..), heapWords0, heapWords1)
import qualified Data.ByteString as BS (ByteString, length)
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import qualified PlutusTx as Plutus
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Metadata (Metadatum)
import Shelley.Spec.Ledger.Serialization (mapFromCBOR)

-- =====================================================================
-- Plutus.Data is the type that Plutus expects as data.
-- It is imported from the Plutus package, but it needs a few additional
-- instances to also work in the ledger.

instance FromCBOR (Annotator Plutus.Data) where
  fromCBOR = decode (Summands "PlutusData" decPlutus)
    where
      decPlutus :: Word -> Decode 'Open (Annotator Plutus.Data)
      decPlutus 0 = Ann (SumD Plutus.Constr) <*! (Ann From) <*! listDecodeA From
      decPlutus 1 =
        Ann (SumD Plutus.Map)
          <*! (D $ decodeMapContentsTraverse fromCBOR fromCBOR)
      decPlutus 2 = Ann (SumD Plutus.List) <*! listDecodeA From
      decPlutus 3 = Ann (SumD Plutus.I <! From)
      decPlutus 4 = Ann (SumD checkPlutusByteString <? From)
      decPlutus n = Invalid n

checkPlutusByteString :: BS.ByteString -> Either String Plutus.Data
checkPlutusByteString s =
  if BS.length s <= 64
    then Right (Plutus.B s)
    else Left ("Plutus Bytestring in Plutus Data has length greater than 64: " ++ show (BS.length s) ++ "\n  " ++ show s)

instance ToCBOR Plutus.Data where
  toCBOR x = encode (encPlutus x)
    where
      encPlutus (Plutus.Constr tag args) = Sum Plutus.Constr 0 !> To tag !> listEncode args
      encPlutus (Plutus.Map pairs) =
        Sum Plutus.Map 1
          !> E encodeFoldableMapPairs pairs
      encPlutus (Plutus.List xs) = Sum Plutus.List 2 !> listEncode xs
      encPlutus (Plutus.I i) = Sum Plutus.I 3 !> To i
      encPlutus (Plutus.B bytes) = Sum Plutus.B 4 !> To bytes

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
hashData d = hashAnnotated d

-- Size of the datum hash attached to the output (could be Nothing)
dataHashSize :: (CC.Crypto c) => StrictMaybe (DataHash c) -> Integer
dataHashSize dh = fromIntegral $ heapWords dh

instance (CC.Crypto c) => HeapWords (StrictMaybe (DataHash c)) where
  heapWords SNothing = heapWords0
  heapWords (SJust a) = heapWords1 a

-- =============================================================================
-- Version without serialized bytes

data AuxiliaryDataRaw era = AuxiliaryDataRaw
  { txMD' :: !(Map Word64 Metadatum),
    scripts' :: !(StrictSeq (Core.Script era)),
    dats' :: !(Set (Data era))
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
  toCBOR (AuxiliaryDataRaw s d m) =
    encode (encodeRaw s d m)

encodeRaw ::
  ( Core.Script era ~ Script era,
    Typeable (Crypto era),
    Typeable era
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Core.Script era) ->
  Set (Data era) ->
  Encode ('Closed 'Sparse) (AuxiliaryDataRaw era)
encodeRaw metadata allScripts dataSet =
  ( Tag 259 $
      Keyed
        (\m tss pss d -> AuxiliaryDataRaw m (StrictSeq.fromList $ tss <> pss) d)
        !> Omit null (Key 0 $ mapEncode metadata)
        !> Omit null (Key 1 $ E (encodeFoldable . mapMaybe getTimelock) timelocks)
        !> Omit null (Key 2 $ E (encodeFoldable . mapMaybe getPlutus) plutusScripts)
        !> Omit null (Key 3 $ setEncode dataSet)
  )
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
              <*! Ann (Emit Set.empty)
          )
      decodeShelleyMA =
        decode
          ( Ann (RecD AuxiliaryDataRaw)
              <*! Ann (D mapFromCBOR)
              <*! ( D $
                      sequence
                        <$> decodeStrictSeq
                          (fmap TimelockScript <$> fromCBOR)
                  )
              <*! Ann (Emit Set.empty)
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
      auxDataField 3 =
        fieldAA
          (\x ad -> ad {dats' = x})
          (setDecodeA From)
      auxDataField n = field (\_ t -> t) (Invalid n)

emptyAuxData :: AuxiliaryDataRaw era
emptyAuxData = AuxiliaryDataRaw mempty mempty mempty

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
  Set (Data era) ->
  AuxiliaryData era
pattern AuxiliaryData {txMD, scripts, dats} <-
  AuxiliaryDataConstr (Memo (AuxiliaryDataRaw txMD scripts dats) _)
  where
    AuxiliaryData m s d =
      AuxiliaryDataConstr
        ( memoBytes
            (encodeRaw m s d)
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
ppAuxiliaryData (AuxiliaryDataConstr (Memo (AuxiliaryDataRaw m s d) _)) =
  ppSexp "AuxiliaryData" [ppMap ppWord64 ppMetadatum m, ppStrictSeq prettyA s, ppSet ppData d]

instance (PrettyA (Core.Script era)) => PrettyA (AuxiliaryData era) where prettyA = ppAuxiliaryData
