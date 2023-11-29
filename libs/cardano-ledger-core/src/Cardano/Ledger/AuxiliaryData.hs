{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.AuxiliaryData (
  AuxiliaryDataHash (..),

  -- * Deprecations
  ValidateAuxiliaryData,
)
where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Hashes (EraIndependentTxAuxData)
import Cardano.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype AuxiliaryDataHash c = AuxiliaryDataHash
  { unsafeAuxiliaryDataHash :: SafeHash c EraIndependentTxAuxData
  }
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData)

deriving instance Crypto c => EncCBOR (AuxiliaryDataHash c)

deriving instance Crypto c => DecCBOR (AuxiliaryDataHash c)

deriving newtype instance Crypto c => ToJSON (AuxiliaryDataHash c)

type ValidateAuxiliaryData era c = ()

{-# DEPRECATED ValidateAuxiliaryData "Use `Cardano.Ledger.Core.EraTxAuxData` instead" #-}
