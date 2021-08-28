{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto, HASH)
import Cardano.Ledger.Hashes (EraIndependentAuxiliaryData)
import Cardano.Ledger.SafeHash (SafeHash, SafeToHash)
import Control.DeepSeq (NFData (..))
import NoThunks.Class (NoThunks (..))

newtype AuxiliaryDataHash crypto = AuxiliaryDataHash
  { unsafeAuxiliaryDataHash :: SafeHash crypto EraIndependentAuxiliaryData
  }

type HashConstraint c = Hash.HashAlgorithm (CC.HASH c)

deriving instance HashConstraint c => Show (AuxiliaryDataHash c)
deriving instance HashConstraint c => Eq (AuxiliaryDataHash c)
deriving instance HashConstraint c => Ord (AuxiliaryDataHash c)
deriving instance HashConstraint c => SafeToHash (AuxiliaryDataHash c)
deriving instance HashConstraint c => NoThunks (AuxiliaryDataHash c)
deriving instance HashConstraint c => NFData (AuxiliaryDataHash c)

deriving instance
  CC.Crypto crypto =>
  ToCBOR (AuxiliaryDataHash crypto)

deriving instance
  CC.Crypto crypto =>
  FromCBOR (AuxiliaryDataHash crypto)

class ValidateAuxiliaryData era c | era -> c where
  hashAuxiliaryData :: Core.AuxiliaryData era -> AuxiliaryDataHash c
  validateAuxiliaryData :: Core.AuxiliaryData era -> Bool
