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

module Cardano.Ledger.ShelleyMA.AuxiliaryData
  ( AuxiliaryData (..),
    pattern AuxiliaryData,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), peekTokenType)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (EraIndependentAuxiliaryData)
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppMap',
    ppMetadatum,
    ppRecord,
    ppStrictSeq,
    ppWord64,
    text,
  )
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash)
import Cardano.Ledger.Serialization (mapFromCBOR, mapToCBOR)
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
import Data.Coders
import Data.Map.Strict (Map)
import Data.MemoBytes
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Shelley.Spec.Ledger.Metadata (Metadatum)

-- =======================================

-- | Raw, un-memoised metadata type
data AuxiliaryDataRaw era = AuxiliaryDataRaw
  { -- | Structured transaction metadata
    txMetadata :: !(Map Word64 Metadatum),
    -- | Pre-images of script hashes found within the TxBody, but which are not
    -- required as witnesses. Examples include:
    -- - Token policy IDs appearing in transaction outputs
    -- - Pool reward account registrations
    auxiliaryScripts :: !(StrictSeq (Core.Script era))
  }
  deriving (Generic)

deriving instance (Core.ChainData (Core.Script era)) => Eq (AuxiliaryDataRaw era)

deriving instance (Core.ChainData (Core.Script era)) => Show (AuxiliaryDataRaw era)

deriving instance
  (Core.ChainData (Core.Script era)) =>
  NoThunks (AuxiliaryDataRaw era)

newtype AuxiliaryData era = AuxiliaryDataWithBytes (MemoBytes (AuxiliaryDataRaw era))
  deriving (Generic, Typeable)
  deriving newtype (ToCBOR, SafeToHash)

instance (c ~ Crypto era) => HashAnnotated (AuxiliaryData era) EraIndependentAuxiliaryData c

deriving newtype instance
  (Era era, Core.ChainData (Core.Script era)) =>
  Eq (AuxiliaryData era)

deriving newtype instance
  (Era era, Core.ChainData (Core.Script era)) =>
  Show (AuxiliaryData era)

deriving newtype instance
  (Era era, Core.ChainData (Core.Script era)) =>
  NoThunks (AuxiliaryData era)

pattern AuxiliaryData ::
  ( Core.AnnotatedData (Core.Script era),
    Ord (Core.Script era)
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Core.Script era) ->
  AuxiliaryData era
pattern AuxiliaryData blob sp <-
  AuxiliaryDataWithBytes (Memo (AuxiliaryDataRaw blob sp) _)
  where
    AuxiliaryData blob sp =
      AuxiliaryDataWithBytes $
        memoBytes
          (encAuxiliaryDataRaw $ AuxiliaryDataRaw blob sp)

{-# COMPLETE AuxiliaryData #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encode AuxiliaryData
encAuxiliaryDataRaw ::
  (Core.AnnotatedData (Core.Script era)) =>
  AuxiliaryDataRaw era ->
  Encode ('Closed 'Dense) (AuxiliaryDataRaw era)
encAuxiliaryDataRaw (AuxiliaryDataRaw blob sp) =
  Rec AuxiliaryDataRaw
    !> E mapToCBOR blob
    !> E encodeFoldable sp

instance
  (Era era, Core.AnnotatedData (Core.Script era)) =>
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
  (Mem (AuxiliaryDataRaw era))
  instance
    ( Era era,
      Core.AnnotatedData (Core.Script era)
    ) =>
    FromCBOR (Annotator (AuxiliaryData era))

-- =================================
-- Pretty printers

ppAuxiliaryData :: PrettyA (Core.Script era) => AuxiliaryData era -> PDoc
ppAuxiliaryData (AuxiliaryDataWithBytes (Memo (AuxiliaryDataRaw m sp) _)) =
  ppRecord
    "AuxiliaryData"
    [ ("metadata", ppMap' (text "Metadata") ppWord64 ppMetadatum m),
      ("auxiliaryscripts", ppStrictSeq prettyA sp)
    ]

instance PrettyA (Core.Script era) => PrettyA (AuxiliaryData era) where
  prettyA = ppAuxiliaryData
