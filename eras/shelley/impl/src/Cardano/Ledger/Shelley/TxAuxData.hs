{-# LANGUAGE DataKinds #-}
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

module Cardano.Ledger.Shelley.TxAuxData (
  ShelleyTxAuxData (ShelleyTxAuxData),
  ShelleyTxAuxDataRaw,
  hashShelleyTxAuxData,

  -- * Re-exports
  Metadatum (..),
  validMetadatum,
)
where

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Binary (Annotator (..), DecCBOR (..), EncCBOR (..))
import qualified Cardano.Ledger.Binary.Plain as Plain (ToCBOR)
import Cardano.Ledger.Core (Era (..), EraTxAuxData (..))
import Cardano.Ledger.Hashes (
  EraIndependentTxAuxData,
  HashAnnotated,
  SafeHash,
  SafeToHash (..),
  hashAnnotated,
 )
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.Metadata (Metadatum (..), validMetadatum)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Typeable (Proxy (..))
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

deriving via
  (Mem ShelleyTxAuxDataRaw era)
  instance
    Era era => DecCBOR (Annotator (ShelleyTxAuxData era))

newtype ShelleyTxAuxData era
  = AuxiliaryDataConstr (MemoBytes ShelleyTxAuxDataRaw era)
  deriving (Eq, Show, Generic)
  deriving newtype (NFData, Plain.ToCBOR, SafeToHash)

instance Memoized ShelleyTxAuxData where
  type RawType ShelleyTxAuxData = ShelleyTxAuxDataRaw

instance EraTxAuxData ShelleyEra where
  type TxAuxData ShelleyEra = ShelleyTxAuxData ShelleyEra

  mkBasicTxAuxData = ShelleyTxAuxData mempty

  metadataTxAuxDataL =
    lensMemoRawType stadrMetadata $ \txAuxDataRaw md -> txAuxDataRaw {stadrMetadata = md}

  -- Calling this partial function will result in compilation error, since ByronEra has
  -- no instance for EraTxOut type class.
  upgradeTxAuxData = error "It is not possible to translate Byron TxOut with 'upgradeTxOut'"

  validateTxAuxData _ (ShelleyTxAuxData m) = all validMetadatum m

  hashTxAuxData metadata =
    AuxiliaryDataHash (makeHashWithExplicitProxys index metadata)
    where
      index = Proxy @EraIndependentTxAuxData

instance EqRaw (ShelleyTxAuxData era)

instance HashAnnotated (ShelleyTxAuxData era) EraIndependentTxAuxData where
  hashAnnotated = getMemoSafeHash

hashShelleyTxAuxData ::
  ShelleyTxAuxData era ->
  SafeHash EraIndependentTxAuxData
hashShelleyTxAuxData = hashAnnotated

pattern ShelleyTxAuxData :: forall era. Era era => Map Word64 Metadatum -> ShelleyTxAuxData era
pattern ShelleyTxAuxData m <-
  (getMemoRawType -> ShelleyTxAuxDataRaw m)
  where
    ShelleyTxAuxData m = mkMemoized $ ShelleyTxAuxDataRaw m

{-# COMPLETE ShelleyTxAuxData #-}

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (ShelleyTxAuxData era)

type instance MemoHashIndex ShelleyTxAuxDataRaw = EraIndependentTxAuxData
