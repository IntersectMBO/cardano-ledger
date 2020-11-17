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
import Cardano.Crypto.Hash (hashWithSerialiser)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.ShelleyMA (MaryOrAllegra, ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Scripts ()
import Codec.CBOR.Decoding (TokenType (TypeListLen, TypeMapLen))
import Control.DeepSeq (deepseq)
import Data.Coders
import Data.Map.Strict (Map)
import Data.MemoBytes
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Shelley.Spec.Ledger.MetaData
  ( MetaDataHash (..),
    MetaDatum,
    ValidateMetadata (..),
    validMetaDatum,
  )

-- | Raw, un-memoised metadata type
data MetadataRaw era = MetadataRaw
  { -- | Unstructured metadata "blob"
    mdBlob :: !(Map Word64 MetaDatum),
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
  Map Word64 MetaDatum ->
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

type instance
  Core.Metadata (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    Metadata (ShelleyMAEra (ma :: MaryOrAllegra) c)

instance
  ( Crypto c,
    Typeable ma,
    Core.AnnotatedData (Core.Script (ShelleyMAEra ma c))
  ) =>
  ValidateMetadata (ShelleyMAEra (ma :: MaryOrAllegra) c)
  where
  hashMetadata = MetaDataHash . hashWithSerialiser toCBOR

  validateMetadata (Metadata blob sp) = deepseq sp $ all validMetaDatum blob

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encode Metadata
encMetadataRaw ::
  (Core.AnnotatedData (Core.Script era)) =>
  MetadataRaw era ->
  Encode ('Closed 'Dense) (MetadataRaw era)
encMetadataRaw (MetadataRaw blob sp) =
  Rec MetadataRaw
    !> To blob
    !> E encodeFoldable sp

instance
  (Era era, Core.AnnotatedData (Core.Script era)) =>
  FromCBOR (Annotator (MetadataRaw era))
  where
  fromCBOR =
    peekTokenType >>= \case
      TypeMapLen ->
        decode
          ( Ann (Emit MetadataRaw)
              <*! Ann From
              <*! Ann (Emit StrictSeq.empty)
          )
      TypeListLen ->
        decode
          ( Ann (RecD MetadataRaw)
              <*! Ann From
              <*! D (sequence <$> decodeStrictSeq fromCBOR)
          )
      _ -> error "Failed to decode Metadata"

deriving via
  (Mem (MetadataRaw era))
  instance
    ( Era era,
      Core.AnnotatedData (Core.Script era)
    ) =>
    FromCBOR (Annotator (Metadata era))
