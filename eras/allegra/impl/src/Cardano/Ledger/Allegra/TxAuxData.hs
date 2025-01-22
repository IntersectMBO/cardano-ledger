{-# LANGUAGE DataKinds #-}
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
  AllegraTxAuxData (AllegraTxAuxData),
  AllegraTxAuxDataRaw,
  metadataAllegraTxAuxDataL,
  AllegraEraTxAuxData (..),
  timelockScriptsAllegraTxAuxDataL,
)
where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Binary (
  Annotator (..),
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
import Cardano.Ledger.Shelley.TxAuxData (Metadatum, ShelleyTxAuxData (..), validMetadatum)
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
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro (Lens')
import NoThunks.Class (NoThunks)

-- =======================================

-- | Raw, un-memoised metadata type
data AllegraTxAuxDataRaw era = AllegraTxAuxDataRaw
  { atadrMetadata :: !(Map Word64 Metadatum)
  -- ^ Structured transaction metadata
  , atadrTimelock :: !(StrictSeq (Timelock era))
  -- ^ Pre-images of script hashes found within the TxBody, but which are not
  -- required as witnesses. Examples include:
  -- - Token policy IDs appearing in transaction outputs
  -- - Pool reward account registrations
  }
  deriving (Generic, Eq)

class EraTxAuxData era => AllegraEraTxAuxData era where
  timelockScriptsTxAuxDataL :: Lens' (TxAuxData era) (StrictSeq (Timelock era))

instance EraTxAuxData AllegraEra where
  type TxAuxData AllegraEra = AllegraTxAuxData AllegraEra

  mkBasicTxAuxData = AllegraTxAuxData mempty mempty

  metadataTxAuxDataL = metadataAllegraTxAuxDataL

  upgradeTxAuxData (ShelleyTxAuxData md) = AllegraTxAuxData md mempty

  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md

metadataAllegraTxAuxDataL ::
  forall era. Era era => Lens' (AllegraTxAuxData era) (Map Word64 Metadatum)
metadataAllegraTxAuxDataL =
  lensMemoRawType @era atadrMetadata $
    \txAuxDataRaw md -> txAuxDataRaw {atadrMetadata = md}

instance AllegraEraTxAuxData AllegraEra where
  timelockScriptsTxAuxDataL = timelockScriptsAllegraTxAuxDataL

timelockScriptsAllegraTxAuxDataL ::
  forall era.
  Era era => Lens' (AllegraTxAuxData era) (StrictSeq (Timelock era))
timelockScriptsAllegraTxAuxDataL =
  lensMemoRawType @era atadrTimelock $
    \txAuxDataRaw ts -> txAuxDataRaw {atadrTimelock = ts}

deriving instance Show (AllegraTxAuxDataRaw era)

deriving instance Era era => NoThunks (AllegraTxAuxDataRaw era)

instance NFData (AllegraTxAuxDataRaw era)

newtype AllegraTxAuxData era = AuxiliaryDataWithBytes (MemoBytes (AllegraTxAuxDataRaw era))
  deriving (Generic)
  deriving newtype (Eq, ToCBOR, SafeToHash)

instance Memoized (AllegraTxAuxData era) where
  type RawType (AllegraTxAuxData era) = AllegraTxAuxDataRaw era

type instance MemoHashIndex (AllegraTxAuxDataRaw era) = EraIndependentTxAuxData

instance HashAnnotated (AllegraTxAuxData era) EraIndependentTxAuxData where
  hashAnnotated = getMemoSafeHash

deriving newtype instance Show (AllegraTxAuxData era)

deriving newtype instance Era era => NoThunks (AllegraTxAuxData era)

deriving newtype instance NFData (AllegraTxAuxData era)

instance EqRaw (AllegraTxAuxData era)

pattern AllegraTxAuxData ::
  forall era.
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Timelock era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData blob sp <- (getMemoRawType -> AllegraTxAuxDataRaw blob sp)
  where
    AllegraTxAuxData blob sp = mkMemoizedEra @era $ AllegraTxAuxDataRaw blob sp

{-# COMPLETE AllegraTxAuxData #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance Era era => EncCBOR (AllegraTxAuxDataRaw era) where
  encCBOR (AllegraTxAuxDataRaw blob sp) =
    encode (Rec AllegraTxAuxDataRaw !> To blob !> To sp)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (AllegraTxAuxData era)

instance Era era => DecCBOR (Annotator (AllegraTxAuxDataRaw era)) where
  decCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeFromMap
      TypeMapLen64 -> decodeFromMap
      TypeMapLenIndef -> decodeFromMap
      TypeListLen -> decodeFromList
      TypeListLen64 -> decodeFromList
      TypeListLenIndef -> decodeFromList
      _ -> error "Failed to decode AuxiliaryData"
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

deriving via
  (Mem (AllegraTxAuxDataRaw era))
  instance
    Era era => DecCBOR (Annotator (AllegraTxAuxData era))
