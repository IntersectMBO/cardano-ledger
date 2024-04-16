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
    AlonzoTxAuxData,
    AlonzoTxAuxData',
    atadMetadata,
    atadTimelock,
    atadPlutus,
    atadMetadata',
    atadTimelock',
    atadPlutus'
  ),
  AlonzoEraTxAuxData (..),
  AlonzoTxAuxDataRaw,
  mkAlonzoTxAuxData,
  AuxiliaryDataHash (..),
  hashAlonzoTxAuxData,
  validateAlonzoTxAuxData,
  getAlonzoTxAuxDataScripts,
  translateAlonzoTxAuxData,
  metadataAlonzoTxAuxDataL,
  timelockScriptsAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,

  -- * Deprecated
  AuxiliaryData,
)
where

import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.Allegra.Scripts (Timelock, translateTimelock)
import Cardano.Ledger.Allegra.TxAuxData (AllegraEraTxAuxData (..), AllegraTxAuxData (..))
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AlonzoScript (..),
  mkBinaryPlutusScript,
  plutusScriptBinary,
  plutusScriptLanguage,
  validScript,
 )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBOR (..),
  ToCBOR,
  TokenType (..),
  decodeStrictSeq,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto (HASH))
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes (..),
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.Plutus.Language (Language (..), PlutusBinary (..), guardPlutus)
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash (..), hashAnnotated)
import Cardano.Ledger.Shelley.TxAuxData (Metadatum, validMetadatum)
import Control.DeepSeq (NFData, deepseq)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro (Lens')
import NoThunks.Class (InspectHeapNamed (..), NoThunks)

class AllegraEraTxAuxData era => AlonzoEraTxAuxData era where
  plutusScriptsTxAuxDataL :: Lens' (TxAuxData era) (Map Language (NE.NonEmpty PlutusBinary))

data AlonzoTxAuxDataRaw era = AlonzoTxAuxDataRaw
  { atadrMetadata :: !(Map Word64 Metadatum)
  , atadrTimelock :: !(StrictSeq (Timelock era))
  , atadrPlutus :: !(Map Language (NE.NonEmpty PlutusBinary))
  }
  deriving (Generic)

deriving instance Eq (Timelock era) => Eq (AlonzoTxAuxDataRaw era)

deriving instance Show (Timelock era) => Show (AlonzoTxAuxDataRaw era)

instance NFData (Timelock era) => NFData (AlonzoTxAuxDataRaw era)

deriving via
  InspectHeapNamed "AlonzoTxAuxDataRaw" (AlonzoTxAuxDataRaw era)
  instance
    NoThunks (AlonzoTxAuxDataRaw era)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (AlonzoTxAuxData era)

instance Era era => EncCBOR (AlonzoTxAuxDataRaw era) where
  encCBOR AlonzoTxAuxDataRaw {atadrMetadata, atadrTimelock, atadrPlutus} =
    encode $
      Tag 259 $
        Keyed
          ( \m ts mps1 mps2 mps3 ->
              AlonzoTxAuxDataRaw m ts $
                Map.fromList
                  [ (pv, ps)
                  | (pv, Just ps) <-
                      [ (PlutusV1, mps1)
                      , (PlutusV2, mps2)
                      , (PlutusV3, mps3)
                      ]
                  ]
          )
          !> Omit null (Key 0 $ To atadrMetadata)
          !> Omit null (Key 1 $ To atadrTimelock)
          !> Omit isNothing (Key 2 $ E (maybe mempty encCBOR) (Map.lookup PlutusV1 atadrPlutus))
          !> Omit isNothing (Key 3 $ E (maybe mempty encCBOR) (Map.lookup PlutusV2 atadrPlutus))
          !> Omit isNothing (Key 4 $ E (maybe mempty encCBOR) (Map.lookup PlutusV3 atadrPlutus))

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
  mkMemoized $ AlonzoTxAuxDataRaw {atadrMetadata, atadrTimelock, atadrPlutus}
  where
    partitionScripts (tss, pss) =
      \case
        TimelockScript ts -> (ts :<| tss, pss)
        PlutusScript ps ->
          let lang = plutusScriptLanguage ps
              bs = plutusScriptBinary ps
           in (tss, Map.alter (Just . maybe (pure bs) (NE.cons bs)) lang pss)
    (atadrTimelock, atadrPlutus) =
      foldr (flip partitionScripts) (mempty, Map.empty) allScripts

getAlonzoTxAuxDataScripts ::
  forall era.
  AlonzoEraScript era =>
  AlonzoTxAuxData era ->
  StrictSeq (AlonzoScript era)
getAlonzoTxAuxDataScripts AlonzoTxAuxData {atadTimelock = timelocks, atadPlutus = plutus} =
  mconcat $
    (TimelockScript <$> timelocks)
      : [ StrictSeq.fromList $
          -- It is fine to filter out unsupported languages with mapMaybe, because the invariant for
          -- AlonzoTxAuxData is that it does not contain scripts with languages that are not
          -- supported in this era
          mapMaybe (fmap PlutusScript . mkBinaryPlutusScript lang) $
            NE.toList plutusScripts
        | lang <- [PlutusV1 .. eraMaxLanguage @era]
        , Just plutusScripts <- [Map.lookup lang plutus]
        ]

instance Era era => DecCBOR (Annotator (AlonzoTxAuxDataRaw era)) where
  decCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeShelley
      TypeMapLen64 -> decodeShelley
      TypeMapLenIndef -> decodeShelley
      TypeListLen -> decodeShelleyMA
      TypeListLen64 -> decodeShelleyMA
      TypeListLenIndef -> decodeShelleyMA
      TypeTag -> decodeAlonzo
      TypeTag64 -> decodeAlonzo
      _ -> fail "Failed to decode AuxiliaryData"
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
                (sequence <$> decodeStrictSeq decCBOR)
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
          (D (sequence <$> decodeStrictSeq decCBOR))
      auxDataField 2 = fieldA (addPlutusScripts PlutusV1) (D (guardPlutus PlutusV1 >> decCBOR))
      auxDataField 3 = fieldA (addPlutusScripts PlutusV2) (D (guardPlutus PlutusV2 >> decCBOR))
      auxDataField 4 = fieldA (addPlutusScripts PlutusV3) (D (guardPlutus PlutusV3 >> decCBOR))
      auxDataField n = field (\_ t -> t) (Invalid n)

emptyAuxData :: AlonzoTxAuxDataRaw era
emptyAuxData = AlonzoTxAuxDataRaw mempty mempty mempty

-- ================================================================================
-- Version with serialized bytes.

newtype AlonzoTxAuxData era = AuxiliaryDataConstr (MemoBytes AlonzoTxAuxDataRaw era)
  deriving (Generic)
  deriving newtype (ToCBOR, SafeToHash)

instance Memoized AlonzoTxAuxData where
  type RawType AlonzoTxAuxData = AlonzoTxAuxDataRaw

instance EqRaw (AlonzoTxAuxData era)

type AuxiliaryData era = AlonzoTxAuxData era

{-# DEPRECATED AuxiliaryData "Use `AlonzoTxAuxData` instead" #-}

instance Crypto c => EraTxAuxData (AlonzoEra c) where
  type TxAuxData (AlonzoEra c) = AlonzoTxAuxData (AlonzoEra c)

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  upgradeTxAuxData (AllegraTxAuxData md scripts) =
    mkMemoized $
      AlonzoTxAuxDataRaw
        { atadrMetadata = md
        , atadrTimelock = translateTimelock <$> scripts
        , atadrPlutus = mempty
        }

  hashTxAuxData = hashAlonzoTxAuxData

  validateTxAuxData = validateAlonzoTxAuxData

metadataAlonzoTxAuxDataL :: Era era => Lens' (AlonzoTxAuxData era) (Map Word64 Metadatum)
metadataAlonzoTxAuxDataL =
  lensMemoRawType atadrMetadata $ \txAuxDataRaw md -> txAuxDataRaw {atadrMetadata = md}

hashAlonzoTxAuxData ::
  (HashAlgorithm (HASH c), HashAnnotated x EraIndependentTxAuxData c) =>
  x ->
  AuxiliaryDataHash c
hashAlonzoTxAuxData x = AuxiliaryDataHash (hashAnnotated x)

validateAlonzoTxAuxData ::
  (AlonzoEraScript era, Script era ~ AlonzoScript era) =>
  ProtVer ->
  AuxiliaryData era ->
  Bool
validateAlonzoTxAuxData pv auxData@AlonzoTxAuxData {atadMetadata = metadata} =
  all validMetadatum metadata
    && all (validScript pv) (getAlonzoTxAuxDataScripts auxData)

instance Crypto c => AllegraEraTxAuxData (AlonzoEra c) where
  timelockScriptsTxAuxDataL = timelockScriptsAlonzoTxAuxDataL

timelockScriptsAlonzoTxAuxDataL ::
  Era era => Lens' (AlonzoTxAuxData era) (StrictSeq (Timelock era))
timelockScriptsAlonzoTxAuxDataL =
  lensMemoRawType atadrTimelock $ \txAuxDataRaw ts -> txAuxDataRaw {atadrTimelock = ts}

instance Crypto c => AlonzoEraTxAuxData (AlonzoEra c) where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL

plutusScriptsAllegraTxAuxDataL ::
  Era era => Lens' (AlonzoTxAuxData era) (Map Language (NE.NonEmpty PlutusBinary))
plutusScriptsAllegraTxAuxDataL =
  lensMemoRawType atadrPlutus $ \txAuxDataRaw ts -> txAuxDataRaw {atadrPlutus = ts}

instance EraCrypto era ~ c => HashAnnotated (AuxiliaryData era) EraIndependentTxAuxData c where
  hashAnnotated = getMemoSafeHash

deriving newtype instance NFData (AuxiliaryData era)

deriving instance Eq (AuxiliaryData era)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (AuxiliaryData era)

type instance MemoHashIndex AlonzoTxAuxDataRaw = EraIndependentTxAuxData

deriving via
  InspectHeapNamed "AlonzoTxAuxDataRaw" (AuxiliaryData era)
  instance
    NoThunks (AuxiliaryData era)

deriving via
  (Mem AlonzoTxAuxDataRaw era)
  instance
    Era era => DecCBOR (Annotator (AuxiliaryData era))

-- | Construct auxiliary data. Make sure not to supply plutus script versions that are not
-- supported in this era, because it will result in a runtime exception. Use
-- `mkAlonzoTxAuxData` instead if you need runtime safety guarantees.
pattern AlonzoTxAuxData ::
  forall era.
  (HasCallStack, AlonzoEraScript era) =>
  Map Word64 Metadatum ->
  StrictSeq (Timelock era) ->
  Map Language (NE.NonEmpty PlutusBinary) ->
  AlonzoTxAuxData era
pattern AlonzoTxAuxData {atadMetadata, atadTimelock, atadPlutus} <-
  (getMemoRawType -> AlonzoTxAuxDataRaw atadMetadata atadTimelock atadPlutus)
  where
    AlonzoTxAuxData atadrMetadata atadrTimelock atadrPlutus =
      let unsupportedScripts =
            Map.filterWithKey (\lang _ -> lang > eraMaxLanguage @era) atadrPlutus
          prefix =
            intercalate "," (show <$> Map.keys unsupportedScripts)
              ++ if Map.size unsupportedScripts > 1 then " languages are" else " language is"
       in if Map.null unsupportedScripts
            then mkMemoized $ AlonzoTxAuxDataRaw {atadrMetadata, atadrTimelock, atadrPlutus}
            else error $ prefix ++ " not supported in " ++ eraName @era

{-# COMPLETE AlonzoTxAuxData #-}

pattern AlonzoTxAuxData' ::
  forall era.
  Map Word64 Metadatum ->
  StrictSeq (Timelock era) ->
  Map Language (NE.NonEmpty PlutusBinary) ->
  AlonzoTxAuxData era
pattern AlonzoTxAuxData' {atadMetadata', atadTimelock', atadPlutus'} <-
  (getMemoRawType -> AlonzoTxAuxDataRaw atadMetadata' atadTimelock' atadPlutus')

translateAlonzoTxAuxData ::
  (AlonzoEraScript era1, AlonzoEraScript era2, EraCrypto era1 ~ EraCrypto era2) =>
  AlonzoTxAuxData era1 ->
  AlonzoTxAuxData era2
translateAlonzoTxAuxData AlonzoTxAuxData {atadMetadata, atadTimelock, atadPlutus} =
  AlonzoTxAuxData
    { atadMetadata = atadMetadata
    , atadTimelock = translateTimelock <$> atadTimelock
    , atadPlutus = atadPlutus
    }
