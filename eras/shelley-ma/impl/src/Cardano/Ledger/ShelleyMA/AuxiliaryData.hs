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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.AuxiliaryData
  ( AllegraTxAuxData (AllegraTxAuxData, AllegraTxAuxData', ..),
    Core.TxAuxData,

    -- * Deprecations
    AuxiliaryData,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Binary (Annotator (..), FromCBOR (..), ToCBOR (..), peekTokenType)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
  ( Era (..),
    EraTxAuxData (hashTxAuxData, validateTxAuxData),
    Script,
  )
import qualified Cardano.Ledger.Core as Core (TxAuxData)
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxAuxData)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), MemoHashIndex, memoBytes)
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash, hashAnnotated)
import Cardano.Ledger.Shelley.Metadata (Metadatum, validMetadatum)
import Cardano.Ledger.ShelleyMA.Era
import Cardano.Ledger.ShelleyMA.Timelocks ()
import Codec.CBOR.Decoding
  ( TokenType
      ( TypeListLen,
        TypeListLen64,
        TypeListLenIndef,
        TypeMapLen,
        TypeMapLen64,
        TypeMapLenIndef
      ),
  )
import Control.DeepSeq
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- =======================================

-- | Raw, un-memoised metadata type
data AuxiliaryDataRaw era = AuxiliaryDataRaw
  { -- | Structured transaction metadata
    txMetadata :: !(Map Word64 Metadatum),
    -- | Pre-images of script hashes found within the TxBody, but which are not
    -- required as witnesses. Examples include:
    -- - Token policy IDs appearing in transaction outputs
    -- - Pool reward account registrations
    auxiliaryScripts :: !(StrictSeq (Script era))
  }
  deriving (Generic)

instance (CC.Crypto c, MAClass ma c) => EraTxAuxData (ShelleyMAEra ma c) where
  type TxAuxData (ShelleyMAEra ma c) = AllegraTxAuxData (ShelleyMAEra ma c)
  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md
  hashTxAuxData aux = AuxiliaryDataHash (hashAnnotated aux)

deriving instance Eq (Script era) => Eq (AuxiliaryDataRaw era)

deriving instance Show (Script era) => Show (AuxiliaryDataRaw era)

deriving instance NoThunks (Script era) => NoThunks (AuxiliaryDataRaw era)

instance NFData (Script era) => NFData (AuxiliaryDataRaw era)

newtype AllegraTxAuxData era = AuxiliaryDataWithBytes (MemoBytes AuxiliaryDataRaw era)
  deriving (Generic)
  deriving newtype (ToCBOR, SafeToHash)

type instance MemoHashIndex AuxiliaryDataRaw = EraIndependentTxAuxData

instance (c ~ EraCrypto era) => HashAnnotated (AllegraTxAuxData era) EraIndependentTxAuxData c where
  hashAnnotated (AuxiliaryDataWithBytes mb) = mbHash mb

deriving newtype instance Eq (AllegraTxAuxData era)

deriving newtype instance (Show (Script era), HashAlgorithm (HASH (EraCrypto era))) => Show (AllegraTxAuxData era)

deriving newtype instance
  (NoThunks (Script era), Era era) =>
  NoThunks (AllegraTxAuxData era)

deriving newtype instance NFData (Script era) => NFData (AllegraTxAuxData era)

pattern AllegraTxAuxData ::
  (ToCBOR (Script era), Era era) =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData blob sp <-
  AuxiliaryDataWithBytes (Memo (AuxiliaryDataRaw blob sp) _)
  where
    AllegraTxAuxData blob sp =
      AuxiliaryDataWithBytes $
        memoBytes
          (encAuxiliaryDataRaw $ AuxiliaryDataRaw blob sp)

{-# COMPLETE AllegraTxAuxData #-}

type AuxiliaryData = AllegraTxAuxData

{-# DEPRECATED AuxiliaryData "Use `AllegraTxAuxData` instead" #-}

pattern AllegraTxAuxData' ::
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData' blob sp <-
  AuxiliaryDataWithBytes (Memo (AuxiliaryDataRaw blob sp) _)

{-# COMPLETE AllegraTxAuxData' #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encode AuxiliaryData
encAuxiliaryDataRaw ::
  ToCBOR (Script era) =>
  AuxiliaryDataRaw era ->
  Encode ('Closed 'Dense) (AuxiliaryDataRaw era)
encAuxiliaryDataRaw (AuxiliaryDataRaw blob sp) =
  Rec AuxiliaryDataRaw !> To blob !> To sp

instance
  (Era era, FromCBOR (Annotator (Script era))) =>
  FromCBOR (Annotator (AuxiliaryDataRaw era))
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
          ( Ann (Emit AuxiliaryDataRaw)
              <*! Ann From
              <*! Ann (Emit StrictSeq.empty)
          )
      decodeFromList =
        decode
          ( Ann (RecD AuxiliaryDataRaw)
              <*! Ann From
              <*! D (sequence <$> fromCBOR)
          )

deriving via
  (Mem AuxiliaryDataRaw era)
  instance
    (Era era, FromCBOR (Annotator (Script era))) =>
    FromCBOR (Annotator (AllegraTxAuxData era))
