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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Alonzo.TxAuxData (
  -- * AlonzoTxAuxData
  AlonzoTxAuxData (
    AlonzoTxAuxData,
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
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), BinaryPlutus (..), validScript)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.Binary (
  Annotator (..),
  EncCBOR,
  FromCBOR (..),
  ToCBOR (..),
  TokenType (..),
  decodeStrictSeq,
  peekTokenType,
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
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (
  HashAnnotated,
  SafeToHash (..),
  hashAnnotated,
 )
import Cardano.Ledger.Shelley.TxAuxData (Metadatum, validMetadatum)
import Control.DeepSeq (NFData, deepseq)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)

-- =============================================================================
-- Version without serialized bytes

data AlonzoTxAuxDataRaw era = AlonzoTxAuxDataRaw
  { atadrMetadata :: !(Map Word64 Metadatum)
  , atadrTimelock :: !(StrictSeq (Timelock era))
  , atadrPlutus :: !(Map Language (NE.NonEmpty BinaryPlutus))
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
            [ (PlutusV1, NE.nonEmpty plutusV1Scripts)
            , (PlutusV2, NE.nonEmpty plutusV2Scripts)
            ]
        ]

getAlonzoTxAuxDataScripts :: Era era => AlonzoTxAuxData era -> StrictSeq (AlonzoScript era)
getAlonzoTxAuxDataScripts AlonzoTxAuxData' {atadTimelock' = timelocks, atadPlutus' = plutus} =
  mconcat $
    (TimelockScript <$> timelocks)
      : [ PlutusScript lang . unBinaryPlutus <$> StrictSeq.fromList (NE.toList plutusScripts)
        | lang <- [PlutusV1 ..]
        , Just plutusScripts <- [Map.lookup lang plutus]
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
            -- Avoid leaks by deepseq, since non empty list is lazy.
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
  deriving newtype (EncCBOR, SafeToHash)

instance Memoized AlonzoTxAuxData where
  type RawType AlonzoTxAuxData = AlonzoTxAuxDataRaw

type AuxiliaryData era = AlonzoTxAuxData era

{-# DEPRECATED AuxiliaryData "Use `AlonzoTxAuxData` instead" #-}

instance Crypto c => EraTxAuxData (AlonzoEra c) where
  type TxAuxData (AlonzoEra c) = AlonzoTxAuxData (AlonzoEra c)
  hashTxAuxData = hashAlonzoTxAuxData
  validateTxAuxData = validateAlonzoTxAuxData

hashAlonzoTxAuxData ::
  (HashAlgorithm (HASH c), HashAnnotated x EraIndependentTxAuxData c) =>
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
