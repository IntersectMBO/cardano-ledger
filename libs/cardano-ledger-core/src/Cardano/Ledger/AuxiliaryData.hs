{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Ledger.AuxiliaryData (
  AuxiliaryDataHash (..),

  -- * Deprecations
  ValidateAuxiliaryData,
)
where

import Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (EraIndependentTxAuxData)
import Cardano.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData (..))
import NoThunks.Class (NoThunks (..))
import GHC.Generics (Generic)

newtype AuxiliaryDataHash c = AuxiliaryDataHash
  { unsafeAuxiliaryDataHash :: SafeHash c EraIndependentTxAuxData
  }
  deriving (Show, Eq, Ord, NoThunks, NFData, Generic)

deriving instance CC.Crypto c => ToCBOR (AuxiliaryDataHash c)

deriving instance CC.Crypto c => FromCBOR (AuxiliaryDataHash c)

type ValidateAuxiliaryData era c = ()

{-# DEPRECATED ValidateAuxiliaryData "Use `Cardano.Ledger.Core.EraTxAuxData` instead" #-}
