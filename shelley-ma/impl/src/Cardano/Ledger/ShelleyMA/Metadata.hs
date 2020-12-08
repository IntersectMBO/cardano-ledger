{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.Metadata
  ( Metadata (..),
    pattern Metadata,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), peekTokenType)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
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
import Shelley.Spec.Ledger.Metadata
  ( Metadatum,
  )
import Shelley.Spec.Ledger.Serialization (mapFromCBOR, mapToCBOR)

-- | Raw, un-memoised metadata type
data MetadataRaw era = MetadataRaw
  { -- | Unstructured metadata "blob"
    mdBlob :: !(Map Word64 Metadatum),
    -- | Pre-images of script hashes found within the TxBody, but which are not
    -- required as witnesses. Examples include:
    -- - Token policy IDs appearing in transaction outputs
    -- - Pool reward account registrations
    mdScriptPreimages :: !(StrictSeq (Core.Script era))
  }
  deriving (Generic)

deriving instance (Core.ChainData (Core.Script era)) => Eq (MetadataRaw era)

deriving instance (Core.ChainData (Core.Script era)) => Show (MetadataRaw era)

deriving instance
  (Core.ChainData (Core.Script era)) =>
  NoThunks (MetadataRaw era)

newtype Metadata era = MetadataWithBytes (MemoBytes (MetadataRaw era))
  deriving (Generic, Typeable)
  deriving newtype (ToCBOR)

deriving newtype instance
  (Era era, Core.ChainData (Core.Script era)) =>
  Eq (Metadata era)

deriving newtype instance
  (Era era, Core.ChainData (Core.Script era)) =>
  Show (Metadata era)

deriving newtype instance
  (Era era, Core.ChainData (Core.Script era)) =>
  NoThunks (Metadata era)

pattern Metadata ::
  ( Core.AnnotatedData (Core.Script era),
    Ord (Core.Script era)
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Core.Script era) ->
  Metadata era
pattern Metadata blob sp <-
  MetadataWithBytes (Memo (MetadataRaw blob sp) _)
  where
    Metadata blob sp =
      MetadataWithBytes $
        memoBytes
          (encMetadataRaw $ MetadataRaw blob sp)

{-# COMPLETE Metadata #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encode Metadata
encMetadataRaw ::
  (Core.AnnotatedData (Core.Script era)) =>
  MetadataRaw era ->
  Encode ( 'Closed 'Dense) (MetadataRaw era)
encMetadataRaw (MetadataRaw blob sp) =
  Rec MetadataRaw
    !> E mapToCBOR blob
    !> E encodeFoldable sp

instance
  (Era era, Core.AnnotatedData (Core.Script era)) =>
  FromCBOR (Annotator (MetadataRaw era))
  where
  fromCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeFromMap
      TypeMapLen64 -> decodeFromMap
      TypeMapLenIndef -> decodeFromMap
      TypeListLen -> decodeFromList
      TypeListLen64 -> decodeFromList
      TypeListLenIndef -> decodeFromList
      _ -> error "Failed to decode Metadata"
    where
      decodeFromMap =
        decode
          ( Ann (Emit MetadataRaw)
              <*! Ann (D mapFromCBOR)
              <*! Ann (Emit StrictSeq.empty)
          )
      decodeFromList =
        decode
          ( Ann (RecD MetadataRaw)
              <*! Ann (D mapFromCBOR)
              <*! D (sequence <$> decodeStrictSeq fromCBOR)
          )

deriving via
  (Mem (MetadataRaw era))
  instance
    ( Era era,
      Core.AnnotatedData (Core.Script era)
    ) =>
    FromCBOR (Annotator (Metadata era))
