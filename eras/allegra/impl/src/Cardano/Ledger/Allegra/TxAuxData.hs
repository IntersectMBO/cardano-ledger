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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxAuxData (
  AllegraTxAuxData (AllegraTxAuxData),
  AllegraTxAuxDataRaw,

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
  DecCBOR (..),
  EncCBOR (..),
  ToCBOR,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core (
  Era (..),
  EraTxAuxData (..),
 )
import Cardano.Ledger.Crypto (Crypto (HASH))
import Cardano.Ledger.Hashes (EraIndependentTxAuxData)
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash, hashAnnotated)
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

instance Crypto c => EraTxAuxData (AllegraEra c) where
  type TxAuxData (AllegraEra c) = AllegraTxAuxData (AllegraEra c)

  upgradeTxAuxData (ShelleyTxAuxData md) = AllegraTxAuxData md mempty

  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md

  hashTxAuxData aux = AuxiliaryDataHash (hashAnnotated aux)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (AllegraTxAuxDataRaw era)

deriving instance Era era => NoThunks (AllegraTxAuxDataRaw era)

instance NFData (AllegraTxAuxDataRaw era)

newtype AllegraTxAuxData era = AuxiliaryDataWithBytes (MemoBytes AllegraTxAuxDataRaw era)
  deriving (Generic)
  deriving newtype (Eq, ToCBOR, SafeToHash)

instance Memoized AllegraTxAuxData where
  type RawType AllegraTxAuxData = AllegraTxAuxDataRaw

type instance MemoHashIndex AllegraTxAuxDataRaw = EraIndependentTxAuxData

instance c ~ EraCrypto era => HashAnnotated (AllegraTxAuxData era) EraIndependentTxAuxData c where
  hashAnnotated = getMemoSafeHash

deriving newtype instance
  HashAlgorithm (HASH (EraCrypto era)) =>
  Show (AllegraTxAuxData era)

deriving newtype instance Era era => NoThunks (AllegraTxAuxData era)

deriving newtype instance NFData (AllegraTxAuxData era)

instance EqRaw (AllegraTxAuxData era)

pattern AllegraTxAuxData ::
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Timelock era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData blob sp <- (getMemoRawType -> AllegraTxAuxDataRaw blob sp)
  where
    AllegraTxAuxData blob sp = mkMemoized $ AllegraTxAuxDataRaw blob sp

{-# COMPLETE AllegraTxAuxData #-}

type AuxiliaryData = AllegraTxAuxData

{-# DEPRECATED AuxiliaryData "Use `AllegraTxAuxData` instead" #-}

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
          ( Pure (Emit AllegraTxAuxDataRaw)
              <!> Pure From
              <!> Pure (Emit StrictSeq.empty)
          )
      decodeFromList =
        decode
          ( Pure (RecD AllegraTxAuxDataRaw)
              <!> Pure From
              <!> D (sequence <$> decCBOR)
          )

deriving via
  (Mem AllegraTxAuxDataRaw era)
  instance
    Era era => DecCBOR (Annotator (AllegraTxAuxData era))
