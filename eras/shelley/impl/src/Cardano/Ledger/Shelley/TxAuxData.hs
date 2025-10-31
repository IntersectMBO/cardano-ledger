{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxAuxData (
  ShelleyTxAuxData (
    MkShelleyTxAuxData,
    ShelleyTxAuxData
  ),
  ShelleyTxAuxDataRaw (..),

  -- * Re-exports
  Metadatum (..),
  validMetadatum,
) where

import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR (..))
import qualified Cardano.Ledger.Binary.Plain as Plain (ToCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Metadata (Metadatum (..), validMetadatum)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

newtype ShelleyTxAuxDataRaw era = ShelleyTxAuxDataRaw
  { stadrMetadata :: Map Word64 Metadatum
  }
  deriving (Eq, Show, Generic)
  deriving newtype (NFData)

deriving via
  InspectHeapNamed "ShelleyTxAuxDataRaw" (ShelleyTxAuxDataRaw era)
  instance
    NoThunks (ShelleyTxAuxDataRaw era)

deriving newtype instance Era era => EncCBOR (ShelleyTxAuxDataRaw era)

deriving newtype instance Era era => DecCBOR (ShelleyTxAuxDataRaw era)

instance Era era => DecCBOR (Annotator (ShelleyTxAuxDataRaw era)) where
  decCBOR = pure <$> decCBOR

deriving via
  InspectHeapNamed "ShelleyTxAuxDataRaw" (ShelleyTxAuxData era)
  instance
    NoThunks (ShelleyTxAuxData era)

newtype ShelleyTxAuxData era
  = MkShelleyTxAuxData (MemoBytes (ShelleyTxAuxDataRaw era))
  deriving (Eq, Show, Generic)
  deriving newtype (NFData, Plain.ToCBOR, SafeToHash)

instance Memoized (ShelleyTxAuxData era) where
  type RawType (ShelleyTxAuxData era) = ShelleyTxAuxDataRaw era

deriving via
  Mem (ShelleyTxAuxDataRaw era)
  instance
    Era era => DecCBOR (Annotator (ShelleyTxAuxData era))

instance EraTxAuxData ShelleyEra where
  type TxAuxData ShelleyEra = ShelleyTxAuxData ShelleyEra

  mkBasicTxAuxData = ShelleyTxAuxData mempty

  metadataTxAuxDataL =
    lensMemoRawType @ShelleyEra stadrMetadata $
      \txAuxDataRaw md -> txAuxDataRaw {stadrMetadata = md}

  validateTxAuxData _ (ShelleyTxAuxData m) = all validMetadatum m

instance EqRaw (ShelleyTxAuxData era)

instance HashAnnotated (ShelleyTxAuxData era) EraIndependentTxAuxData where
  hashAnnotated = getMemoSafeHash

pattern ShelleyTxAuxData :: forall era. Era era => Map Word64 Metadatum -> ShelleyTxAuxData era
pattern ShelleyTxAuxData m <-
  (getMemoRawType -> ShelleyTxAuxDataRaw m)
  where
    ShelleyTxAuxData m = mkMemoizedEra @era $ ShelleyTxAuxDataRaw m

{-# COMPLETE ShelleyTxAuxData #-}

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (ShelleyTxAuxData era)

type instance MemoHashIndex (ShelleyTxAuxDataRaw era) = EraIndependentTxAuxData
