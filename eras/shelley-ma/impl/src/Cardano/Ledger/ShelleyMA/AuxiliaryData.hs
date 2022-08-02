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
  ( MAAuxiliaryData (MAAuxiliaryData, AuxiliaryData', ..),
    AuxiliaryData,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), peekTokenType)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Core
  ( Era (..),
    EraAuxiliaryData (hashAuxiliaryData, validateAuxiliaryData),
    Script,
  )
import qualified Cardano.Ledger.Core as Core (AuxiliaryData)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentAuxiliaryData)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash, hashAnnotated)
import Cardano.Ledger.Serialization (mapFromCBOR, mapToCBOR)
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
import Data.Coders
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

instance (CC.Crypto c, MAClass ma c) => EraAuxiliaryData (ShelleyMAEra ma c) where
  type AuxiliaryData (ShelleyMAEra ma c) = MAAuxiliaryData (ShelleyMAEra ma c)
  validateAuxiliaryData _ (MAAuxiliaryData md as) = as `deepseq` all validMetadatum md
  hashAuxiliaryData aux = AuxiliaryDataHash (hashAnnotated aux)

deriving instance Eq (Script era) => Eq (AuxiliaryDataRaw era)

deriving instance Show (Script era) => Show (AuxiliaryDataRaw era)

deriving instance NoThunks (Script era) => NoThunks (AuxiliaryDataRaw era)

instance NFData (Script era) => NFData (AuxiliaryDataRaw era)

newtype MAAuxiliaryData era = AuxiliaryDataWithBytes (MemoBytes AuxiliaryDataRaw era)
  deriving (Generic)
  deriving newtype (ToCBOR, SafeToHash)

instance (c ~ Crypto era) => HashAnnotated (MAAuxiliaryData era) EraIndependentAuxiliaryData c

deriving newtype instance Eq (MAAuxiliaryData era)

deriving newtype instance Show (Script era) => Show (MAAuxiliaryData era)

deriving newtype instance
  (NoThunks (Script era), Era era) =>
  NoThunks (MAAuxiliaryData era)

deriving newtype instance NFData (Script era) => NFData (MAAuxiliaryData era)

pattern MAAuxiliaryData ::
  (ToCBOR (Script era), Era era) =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  MAAuxiliaryData era
pattern MAAuxiliaryData blob sp <-
  AuxiliaryDataWithBytes (Memo (AuxiliaryDataRaw blob sp) _)
  where
    MAAuxiliaryData blob sp =
      AuxiliaryDataWithBytes $
        memoBytes
          (encAuxiliaryDataRaw $ AuxiliaryDataRaw blob sp)

{-# COMPLETE MAAuxiliaryData #-}

type AuxiliaryData = MAAuxiliaryData

{-# DEPRECATED AuxiliaryData "Use `MAAuxiliaryData` instead" #-}

pattern AuxiliaryData' ::
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  MAAuxiliaryData era
pattern AuxiliaryData' blob sp <-
  AuxiliaryDataWithBytes (Memo (AuxiliaryDataRaw blob sp) _)

{-# COMPLETE AuxiliaryData' #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encode AuxiliaryData
encAuxiliaryDataRaw ::
  ToCBOR (Script era) =>
  AuxiliaryDataRaw era ->
  Encode ('Closed 'Dense) (AuxiliaryDataRaw era)
encAuxiliaryDataRaw (AuxiliaryDataRaw blob sp) =
  Rec AuxiliaryDataRaw
    !> E mapToCBOR blob
    !> E encodeFoldable sp

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
              <*! Ann (D mapFromCBOR)
              <*! Ann (Emit StrictSeq.empty)
          )
      decodeFromList =
        decode
          ( Ann (RecD AuxiliaryDataRaw)
              <*! Ann (D mapFromCBOR)
              <*! D (sequence <$> decodeStrictSeq fromCBOR)
          )

deriving via
  (Mem AuxiliaryDataRaw era)
  instance
    (Era era, FromCBOR (Annotator (Script era))) =>
    FromCBOR (Annotator (MAAuxiliaryData era))
