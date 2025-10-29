{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Alonzo.TxAuxData (
  -- * AlonzoTxAuxData
  AlonzoTxAuxData (
    MkAlonzoTxAuxData,
    AlonzoTxAuxData,
    AlonzoTxAuxData',
    atadMetadata,
    atadNativeScripts,
    atadPlutus,
    atadMetadata',
    atadNativeScripts',
    atadPlutus'
  ),
  AlonzoEraTxAuxData (..),
  AlonzoTxAuxDataRaw (..),
  mkAlonzoTxAuxData,
  validateAlonzoTxAuxData,
  getAlonzoTxAuxDataScripts,
  metadataAlonzoTxAuxDataL,
  nativeScriptsAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,
  addPlutusScripts,
  decodeTxAuxDataByTokenType,
  emptyAlonzoTxAuxDataRaw,
) where

import Cardano.Ledger.Allegra.TxAuxData (AllegraEraTxAuxData (..))
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AlonzoScript (..),
  mkBinaryPlutusScript,
  plutusScriptBinary,
  plutusScriptLanguage,
  validScript,
 )
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  ToCBOR,
  TokenType (..),
  decodeStrictSeq,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes (..),
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Plutus.Language (Language (..), PlutusBinary (..), guardPlutus)
import Cardano.Ledger.Shelley.TxAuxData (Metadatum, validMetadatum)
import Control.DeepSeq (NFData, deepseq)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro (Lens')
import NoThunks.Class (InspectHeapNamed (..), NoThunks)

class AllegraEraTxAuxData era => AlonzoEraTxAuxData era where
  plutusScriptsTxAuxDataL :: Lens' (TxAuxData era) (Map Language (NE.NonEmpty PlutusBinary))

data AlonzoTxAuxDataRaw era = AlonzoTxAuxDataRaw
  { atadrMetadata :: !(Map Word64 Metadatum)
  , atadrNativeScripts :: !(StrictSeq (NativeScript era))
  , atadrPlutus :: !(Map Language (NE.NonEmpty PlutusBinary))
  }
  deriving (Generic)

deriving instance Eq (NativeScript era) => Eq (AlonzoTxAuxDataRaw era)

deriving instance Show (NativeScript era) => Show (AlonzoTxAuxDataRaw era)

instance NFData (NativeScript era) => NFData (AlonzoTxAuxDataRaw era)

deriving via
  InspectHeapNamed "AlonzoTxAuxDataRaw" (AlonzoTxAuxDataRaw era)
  instance
    NoThunks (AlonzoTxAuxDataRaw era)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (AlonzoTxAuxData era)

instance (Era era, EncCBOR (NativeScript era)) => EncCBOR (AlonzoTxAuxDataRaw era) where
  encCBOR AlonzoTxAuxDataRaw {atadrMetadata, atadrNativeScripts, atadrPlutus} =
    encode $
      Tag 259 $
        Keyed
          ( \m ts mps1 mps2 mps3 mps4 ->
              AlonzoTxAuxDataRaw m ts $
                Map.fromList
                  [ (pv, ps)
                  | (pv, Just ps) <-
                      [ (PlutusV1, mps1)
                      , (PlutusV2, mps2)
                      , (PlutusV3, mps3)
                      , (PlutusV4, mps4)
                      ]
                  ]
          )
          !> Omit null (Key 0 $ To atadrMetadata)
          !> Omit null (Key 1 $ To atadrNativeScripts)
          !> Omit isNothing (Key 2 $ E (maybe mempty encCBOR) (Map.lookup PlutusV1 atadrPlutus))
          !> Omit isNothing (Key 3 $ E (maybe mempty encCBOR) (Map.lookup PlutusV2 atadrPlutus))
          !> Omit isNothing (Key 4 $ E (maybe mempty encCBOR) (Map.lookup PlutusV3 atadrPlutus))
          !> Omit isNothing (Key 5 $ E (maybe mempty encCBOR) (Map.lookup PlutusV4 atadrPlutus))

-- | Helper function that will construct Auxiliary data from Metadatum map and a list of scripts.
--
-- Note that the relative order of same type scripts will be preserved.
mkAlonzoTxAuxData ::
  forall f era.
  (Foldable f, AlonzoEraScript era) =>
  Map Word64 Metadatum ->
  f (AlonzoScript era) ->
  AlonzoTxAuxData era
mkAlonzoTxAuxData atadrMetadata allScripts =
  mkMemoizedEra @era $
    AlonzoTxAuxDataRaw {atadrMetadata, atadrNativeScripts, atadrPlutus}
  where
    partitionScripts (tss, pss) =
      \case
        NativeScript ts -> (ts :<| tss, pss)
        PlutusScript ps ->
          let lang = plutusScriptLanguage ps
              bs = plutusScriptBinary ps
           in (tss, Map.alter (Just . maybe (pure bs) (NE.cons bs)) lang pss)
    (atadrNativeScripts, atadrPlutus) =
      foldr (flip partitionScripts) (mempty, Map.empty) allScripts

getAlonzoTxAuxDataScripts ::
  forall era.
  AlonzoEraScript era =>
  AlonzoTxAuxData era ->
  StrictSeq (AlonzoScript era)
getAlonzoTxAuxDataScripts AlonzoTxAuxData {atadNativeScripts = timelocks, atadPlutus = plutus} =
  mconcat $
    (NativeScript <$> timelocks)
      : [ StrictSeq.fromList $
            -- It is fine to filter out unsupported languages with mapMaybe, because the invariant for
            -- AlonzoTxAuxData is that it does not contain scripts with languages that are not
            -- supported in this era
            mapMaybe (fmap PlutusScript . mkBinaryPlutusScript lang) $
              NE.toList plutusScripts
        | lang <- [PlutusV1 .. eraMaxLanguage @era]
        , Just plutusScripts <- [Map.lookup lang plutus]
        ]

instance
  ( Era era
  , DecCBOR (Annotator (NativeScript era))
  , Typeable (NativeScript era)
  ) =>
  DecCBOR (Annotator (AlonzoTxAuxDataRaw era))
  where
  decCBOR =
    decodeTxAuxDataByTokenType @(Annotator (AlonzoTxAuxDataRaw era))
      decodeShelley
      decodeAllegra
      decodeAlonzo
    where
      decodeShelley =
        decode
          ( Ann (Emit AlonzoTxAuxDataRaw)
              <*! Ann From
              <*! Ann (Emit StrictSeq.empty)
              <*! Ann (Emit Map.empty)
          )
      decodeAllegra =
        decode
          ( Ann (RecD AlonzoTxAuxDataRaw)
              <*! Ann From
              <*! D
                (sequence <$> decodeStrictSeq decCBOR)
              <*! Ann (Emit Map.empty)
          )
      decodeAlonzo =
        decode $
          TagD 259 $
            SparseKeyed "AlonzoTxAuxData" (pure emptyAlonzoTxAuxDataRaw) auxDataField []

      auxDataField :: Word -> Field (Annotator (AlonzoTxAuxDataRaw era))
      auxDataField 0 = fieldA (\x ad -> ad {atadrMetadata = x}) From
      auxDataField 1 =
        fieldAA
          (\x ad -> ad {atadrNativeScripts = atadrNativeScripts ad <> x})
          (D (sequence <$> decodeStrictSeq decCBOR))
      auxDataField 2 = fieldA (addPlutusScripts PlutusV1) (D (guardPlutus PlutusV1 >> decCBOR))
      auxDataField 3 = fieldA (addPlutusScripts PlutusV2) (D (guardPlutus PlutusV2 >> decCBOR))
      auxDataField 4 = fieldA (addPlutusScripts PlutusV3) (D (guardPlutus PlutusV3 >> decCBOR))
      auxDataField 5 = fieldA (addPlutusScripts PlutusV4) (D (guardPlutus PlutusV4 >> decCBOR))
      auxDataField n = invalidField n

decodeTxAuxDataByTokenType :: forall t s. Decoder s t -> Decoder s t -> Decoder s t -> Decoder s t
decodeTxAuxDataByTokenType decodeShelley decodeAllegra decodeAlonzo =
  peekTokenType >>= \case
    TypeMapLen -> decodeShelley
    TypeMapLen64 -> decodeShelley
    TypeMapLenIndef -> decodeShelley
    TypeListLen -> decodeAllegra
    TypeListLen64 -> decodeAllegra
    TypeListLenIndef -> decodeAllegra
    TypeTag -> decodeAlonzo
    TypeTag64 -> decodeAlonzo
    _ -> fail "Failed to decode AlonzoTxAuxData"

addPlutusScripts :: Language -> [PlutusBinary] -> AlonzoTxAuxDataRaw era -> AlonzoTxAuxDataRaw era
addPlutusScripts lang scripts ad =
  case NE.nonEmpty scripts of
    Nothing -> ad
    Just neScripts ->
      -- Avoid leaks by deepseq, since non empty list is lazy.
      neScripts `deepseq` ad {atadrPlutus = Map.insert lang neScripts $ atadrPlutus ad}

emptyAlonzoTxAuxDataRaw :: AlonzoTxAuxDataRaw era
emptyAlonzoTxAuxDataRaw = AlonzoTxAuxDataRaw mempty mempty mempty

-- ================================================================================
-- Version with serialized bytes.

newtype AlonzoTxAuxData era = MkAlonzoTxAuxData (MemoBytes (AlonzoTxAuxDataRaw era))
  deriving (Generic)
  deriving newtype (ToCBOR, SafeToHash)

instance Memoized (AlonzoTxAuxData era) where
  type RawType (AlonzoTxAuxData era) = AlonzoTxAuxDataRaw era

deriving via
  Mem (AlonzoTxAuxDataRaw era)
  instance
    (Era era, DecCBOR (Annotator (NativeScript era)), Typeable (NativeScript era)) =>
    DecCBOR (Annotator (AlonzoTxAuxData era))

instance Eq (NativeScript era) => EqRaw (AlonzoTxAuxData era)

instance EraTxAuxData AlonzoEra where
  type TxAuxData AlonzoEra = AlonzoTxAuxData AlonzoEra

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  validateTxAuxData = validateAlonzoTxAuxData

metadataAlonzoTxAuxDataL ::
  forall era.
  (Era era, EncCBOR (NativeScript era)) => Lens' (AlonzoTxAuxData era) (Map Word64 Metadatum)
metadataAlonzoTxAuxDataL =
  lensMemoRawType @era atadrMetadata $
    \txAuxDataRaw md -> txAuxDataRaw {atadrMetadata = md}

validateAlonzoTxAuxData ::
  (AlonzoEraScript era, Script era ~ AlonzoScript era) =>
  ProtVer ->
  AlonzoTxAuxData era ->
  Bool
validateAlonzoTxAuxData pv auxData@AlonzoTxAuxData {atadMetadata = metadata} =
  all validMetadatum metadata
    && all (validScript pv) (getAlonzoTxAuxDataScripts auxData)

instance AllegraEraTxAuxData AlonzoEra where
  nativeScriptsTxAuxDataL = nativeScriptsAlonzoTxAuxDataL

nativeScriptsAlonzoTxAuxDataL ::
  forall era.
  (Era era, EncCBOR (NativeScript era)) => Lens' (AlonzoTxAuxData era) (StrictSeq (NativeScript era))
nativeScriptsAlonzoTxAuxDataL =
  lensMemoRawType @era atadrNativeScripts $
    \txAuxDataRaw ts -> txAuxDataRaw {atadrNativeScripts = ts}

instance AlonzoEraTxAuxData AlonzoEra where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL

plutusScriptsAllegraTxAuxDataL ::
  forall era.
  (Era era, EncCBOR (NativeScript era)) =>
  Lens' (AlonzoTxAuxData era) (Map Language (NE.NonEmpty PlutusBinary))
plutusScriptsAllegraTxAuxDataL =
  lensMemoRawType @era atadrPlutus $
    \txAuxDataRaw ts -> txAuxDataRaw {atadrPlutus = ts}

instance HashAnnotated (AlonzoTxAuxData era) EraIndependentTxAuxData where
  hashAnnotated = getMemoSafeHash

deriving newtype instance NFData (NativeScript era) => NFData (AlonzoTxAuxData era)

deriving instance Eq (NativeScript era) => Eq (AlonzoTxAuxData era)

deriving instance Show (NativeScript era) => Show (AlonzoTxAuxData era)

type instance MemoHashIndex (AlonzoTxAuxDataRaw era) = EraIndependentTxAuxData

deriving via
  InspectHeapNamed "AlonzoTxAuxDataRaw" (AlonzoTxAuxData era)
  instance
    NoThunks (AlonzoTxAuxData era)

-- | Construct auxiliary data. Make sure not to supply plutus script versions that are not
-- supported in this era, because it will result in a runtime exception. Use
-- `mkAlonzoTxAuxData` instead if you need runtime safety guarantees.
pattern AlonzoTxAuxData ::
  forall era.
  (HasCallStack, AlonzoEraScript era) =>
  Map Word64 Metadatum ->
  StrictSeq (NativeScript era) ->
  Map Language (NE.NonEmpty PlutusBinary) ->
  AlonzoTxAuxData era
pattern AlonzoTxAuxData {atadMetadata, atadNativeScripts, atadPlutus} <-
  (getMemoRawType -> AlonzoTxAuxDataRaw atadMetadata atadNativeScripts atadPlutus)
  where
    AlonzoTxAuxData atadrMetadata atadrNativeScripts atadrPlutus =
      let unsupportedScripts =
            Map.filterWithKey (\lang _ -> lang > eraMaxLanguage @era) atadrPlutus
          prefix =
            intercalate "," (show <$> Map.keys unsupportedScripts)
              ++ if Map.size unsupportedScripts > 1 then " languages are" else " language is"
       in if Map.null unsupportedScripts
            then
              mkMemoizedEra @era $ AlonzoTxAuxDataRaw {atadrMetadata, atadrNativeScripts, atadrPlutus}
            else error $ prefix ++ " not supported in " ++ eraName @era

{-# COMPLETE AlonzoTxAuxData #-}

pattern AlonzoTxAuxData' ::
  forall era.
  Map Word64 Metadatum ->
  StrictSeq (NativeScript era) ->
  Map Language (NE.NonEmpty PlutusBinary) ->
  AlonzoTxAuxData era
pattern AlonzoTxAuxData' {atadMetadata', atadNativeScripts', atadPlutus'} <-
  (getMemoRawType -> AlonzoTxAuxDataRaw atadMetadata' atadNativeScripts' atadPlutus')
