{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),

    -- * Deprecations
    ValidateAuxiliaryData,
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (EraIndependentAuxiliaryData)
import Cardano.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData (..))
import NoThunks.Class (NoThunks (..))

newtype AuxiliaryDataHash crypto = AuxiliaryDataHash
  { unsafeAuxiliaryDataHash :: SafeHash crypto EraIndependentAuxiliaryData
  }
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving instance CC.Crypto crypto => ToCBOR (AuxiliaryDataHash crypto)

deriving instance CC.Crypto crypto => FromCBOR (AuxiliaryDataHash crypto)

type ValidateAuxiliaryData era c = ()

{-# DEPRECATED ValidateAuxiliaryData "Use `Cardano.Ledger.Core.EraAuxiliaryData` instead" #-}
