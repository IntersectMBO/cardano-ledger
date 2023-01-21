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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxAuxData (
  AllegraTxAuxData (AllegraTxAuxData, AllegraTxAuxData', ..),

  -- * Deprecations
  AuxiliaryData,
)
where

import Cardano.Crypto.Hash (HashAlgorithm)
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Binary (
  Annotator (..),
  EncCBOR (encCBOR),
  FromCBOR (..),
  ToCBOR (..),
  encodeStrictSeq,
  fromPlainEncoding,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core (
  Era (..),
  EraTxAuxData (hashTxAuxData, validateTxAuxData),
  Script,
 )
import qualified Cardano.Ledger.Core as Core (TxAuxData)
import Cardano.Ledger.Crypto (Crypto (HASH))
import Cardano.Ledger.Hashes (EraIndependentTxAuxData)
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash, hashAnnotated)
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
import Data.Word (Word64)
import GHC.Generics (Generic)
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

instance (Crypto c) => EraTxAuxData (AllegraEra c) where
  type TxAuxData (AllegraEra c) = AllegraTxAuxData (AllegraEra c)
  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md
  hashTxAuxData aux = AuxiliaryDataHash (hashAnnotated aux)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (AllegraTxAuxDataRaw era)

deriving instance Era era => NoThunks (AllegraTxAuxDataRaw era)

instance NFData (AllegraTxAuxDataRaw era)

newtype AllegraTxAuxData era = AuxiliaryDataWithBytes (MemoBytes AllegraTxAuxDataRaw era)
  deriving (Generic)
  deriving newtype (Eq, EncCBOR, SafeToHash)

instance Memoized AllegraTxAuxData where
  type RawType AllegraTxAuxData = AllegraTxAuxDataRaw

type instance MemoHashIndex AllegraTxAuxDataRaw = EraIndependentTxAuxData

instance (c ~ EraCrypto era) => HashAnnotated (AllegraTxAuxData era) EraIndependentTxAuxData c where
  hashAnnotated = getMemoSafeHash

deriving newtype instance
  HashAlgorithm (HASH (EraCrypto era)) =>
  Show (AllegraTxAuxData era)

deriving newtype instance Era era => NoThunks (AllegraTxAuxData era)

deriving newtype instance NFData (AllegraTxAuxData era)

pattern AllegraTxAuxData ::
  (ToCBOR (Script era), Era era) =>
  Map Word64 Metadatum ->
  StrictSeq (Timelock era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData blob sp <- (getMemoRawType -> AllegraTxAuxDataRaw blob sp)
  where
    AllegraTxAuxData blob sp = mkMemoized $ AllegraTxAuxDataRaw blob sp

{-# COMPLETE AllegraTxAuxData #-}

type AuxiliaryData = AllegraTxAuxData

{-# DEPRECATED AuxiliaryData "Use `AllegraTxAuxData` instead" #-}

pattern AllegraTxAuxData' ::
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Timelock era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData' blob sp <-
  (getMemoRawType -> AllegraTxAuxDataRaw blob sp)

{-# COMPLETE AllegraTxAuxData' #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance (Era era, ToCBOR (Script era)) => ToCBOR (AllegraTxAuxDataRaw era) where
  toCBOR (AllegraTxAuxDataRaw blob sp) =
    encode (Rec AllegraTxAuxDataRaw !> To blob !> E (encodeStrictSeq (fromPlainEncoding . encCBOR)) sp)

instance
  (Era era, FromCBOR (Annotator (Script era))) =>
  FromCBOR (Annotator (AllegraTxAuxDataRaw era))
  where
  fromCBOR =
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
              <*! D (sequence <$> fromCBOR)
          )

deriving via
  (Mem AllegraTxAuxDataRaw era)
  instance
    (Era era, FromCBOR (Annotator (Script era))) =>
    FromCBOR (Annotator (AllegraTxAuxData era))
