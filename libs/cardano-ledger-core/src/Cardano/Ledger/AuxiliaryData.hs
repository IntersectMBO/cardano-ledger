{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Ledger.BaseTypes (ProtVer)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (EraIndependentAuxiliaryData)
import Cardano.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData (..))
import Data.Compact.HashMap (Keyed)
import NoThunks.Class (NoThunks (..))

newtype AuxiliaryDataHash crypto = AuxiliaryDataHash
  { unsafeAuxiliaryDataHash :: SafeHash crypto EraIndependentAuxiliaryData
  }
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving newtype instance
  CC.Crypto crypto =>
  Keyed (AuxiliaryDataHash crypto)

deriving instance
  CC.Crypto crypto =>
  ToCBOR (AuxiliaryDataHash crypto)

deriving instance
  CC.Crypto crypto =>
  FromCBOR (AuxiliaryDataHash crypto)

class ValidateAuxiliaryData era c | era -> c where
  hashAuxiliaryData :: Core.AuxiliaryData era -> AuxiliaryDataHash c
  validateAuxiliaryData :: ProtVer -> Core.AuxiliaryData era -> Bool
