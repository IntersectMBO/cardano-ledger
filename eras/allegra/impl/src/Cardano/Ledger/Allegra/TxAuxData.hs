{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxAuxData (
  AllegraTxAuxData (AllegraTxAuxData, MkAlegraTxAuxData),
  AllegraTxAuxDataRaw (..),
  metadataAllegraTxAuxDataL,
  AllegraEraTxAuxData (..),
  nativeScriptsAllegraTxAuxDataL,
) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (AllegraEraScript)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBOR (..),
  ToCBOR,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.TxAuxData (Metadatum, validMetadatum)
import Codec.CBOR.Decoding (
  TokenType (
    TypeListLen,
    TypeListLen64,
    TypeListLenIndef,
    TypeMapLen,
    TypeMapLen64,
    TypeMapLenIndef
  ),
 )
import Control.DeepSeq (NFData, deepseq)
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro (Lens')
import NoThunks.Class (NoThunks)

-- =======================================

-- | Raw, un-memoised metadata type
data AllegraTxAuxDataRaw era = AllegraTxAuxDataRaw
  { atadrMetadata :: !(Map Word64 Metadatum)
  -- ^ Structured transaction metadata
  , atadrNativeScripts :: !(StrictSeq (NativeScript era))
  -- ^ Pre-images of script hashes found within the TxBody, but which are not
  -- required as witnesses. Examples include:
  -- - Token policy IDs appearing in transaction outputs
  -- - Pool account address registrations
  }
  deriving (Generic)

deriving instance Eq (NativeScript era) => Eq (AllegraTxAuxDataRaw era)

class EraTxAuxData era => AllegraEraTxAuxData era where
  nativeScriptsTxAuxDataL :: Lens' (TxAuxData era) (StrictSeq (NativeScript era))

instance EraTxAuxData AllegraEra where
  type TxAuxData AllegraEra = AllegraTxAuxData AllegraEra

  mkBasicTxAuxData = AllegraTxAuxData mempty mempty

  metadataTxAuxDataL = metadataAllegraTxAuxDataL

  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md

metadataAllegraTxAuxDataL ::
  forall era.
  ( Era era
  , EncCBOR (NativeScript era)
  ) =>
  Lens' (AllegraTxAuxData era) (Map Word64 Metadatum)
metadataAllegraTxAuxDataL =
  lensMemoRawType @era atadrMetadata $
    \txAuxDataRaw md -> txAuxDataRaw {atadrMetadata = md}

instance AllegraEraTxAuxData AllegraEra where
  nativeScriptsTxAuxDataL = nativeScriptsAllegraTxAuxDataL

nativeScriptsAllegraTxAuxDataL ::
  forall era.
  (Era era, EncCBOR (NativeScript era)) =>
  Lens' (AllegraTxAuxData era) (StrictSeq (NativeScript era))
nativeScriptsAllegraTxAuxDataL =
  lensMemoRawType @era atadrNativeScripts $
    \txAuxDataRaw ts -> txAuxDataRaw {atadrNativeScripts = ts}

deriving instance Show (NativeScript era) => Show (AllegraTxAuxDataRaw era)

deriving instance (Era era, NoThunks (NativeScript era)) => NoThunks (AllegraTxAuxDataRaw era)

instance NFData (NativeScript era) => NFData (AllegraTxAuxDataRaw era)

newtype AllegraTxAuxData era = MkAlegraTxAuxData (MemoBytes (AllegraTxAuxDataRaw era))
  deriving (Generic)
  deriving newtype (ToCBOR, SafeToHash)

deriving instance Eq (NativeScript era) => Eq (AllegraTxAuxData era)

instance Memoized (AllegraTxAuxData era) where
  type RawType (AllegraTxAuxData era) = AllegraTxAuxDataRaw era

deriving via
  (Mem (AllegraTxAuxDataRaw era))
  instance
    AllegraEraScript era => DecCBOR (Annotator (AllegraTxAuxData era))

type instance MemoHashIndex (AllegraTxAuxDataRaw era) = EraIndependentTxAuxData

instance HashAnnotated (AllegraTxAuxData era) EraIndependentTxAuxData where
  hashAnnotated = getMemoSafeHash

deriving newtype instance Show (NativeScript era) => Show (AllegraTxAuxData era)

deriving newtype instance (Era era, NoThunks (NativeScript era)) => NoThunks (AllegraTxAuxData era)

deriving newtype instance NFData (NativeScript era) => NFData (AllegraTxAuxData era)

instance Eq (NativeScript era) => EqRaw (AllegraTxAuxData era)

pattern AllegraTxAuxData ::
  forall era.
  ( Era era
  , EncCBOR (NativeScript era)
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (NativeScript era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData blob sp <- (getMemoRawType -> AllegraTxAuxDataRaw blob sp)
  where
    AllegraTxAuxData blob sp = mkMemoizedEra @era $ AllegraTxAuxDataRaw blob sp

{-# COMPLETE AllegraTxAuxData #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance (Era era, EncCBOR (NativeScript era)) => EncCBOR (AllegraTxAuxDataRaw era) where
  encCBOR (AllegraTxAuxDataRaw blob sp) =
    encode (Rec AllegraTxAuxDataRaw !> To blob !> To sp)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (AllegraTxAuxData era)

instance
  (Era era, Typeable (NativeScript era), DecCBOR (Annotator (NativeScript era))) =>
  DecCBOR (Annotator (AllegraTxAuxDataRaw era))
  where
  decCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeFromMap
      TypeMapLen64 -> decodeFromMap
      TypeMapLenIndef -> decodeFromMap
      TypeListLen -> decodeFromList
      TypeListLen64 -> decodeFromList
      TypeListLenIndef -> decodeFromList
      _ -> fail "Failed to decode AuxiliaryDataRaw"
    where
      decodeFromMap =
        decode
          ( Ann (Emit AllegraTxAuxDataRaw)
              <*! Ann From
              <*! Ann (Emit StrictSeq.empty)
          )
      decodeFromList =
        decode
          ( Ann (RecD AllegraTxAuxDataRaw)
              <*! Ann From
              <*! D (sequence <$> decCBOR)
          )
