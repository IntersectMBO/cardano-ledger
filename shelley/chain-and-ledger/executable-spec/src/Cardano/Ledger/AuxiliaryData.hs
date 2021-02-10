{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.SafeHash
  ( EraIndependentAuxiliaryData,
    SafeHash,
  )
import Control.DeepSeq (NFData (..))
import NoThunks.Class (NoThunks (..))

newtype AuxiliaryDataHash crypto = AuxiliaryDataHash
  { unsafeAuxiliaryDataHash :: SafeHash crypto EraIndependentAuxiliaryData
  }
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving instance
  CC.Crypto crypto =>
  ToCBOR (AuxiliaryDataHash crypto)

deriving instance
  CC.Crypto crypto =>
  FromCBOR (AuxiliaryDataHash crypto)

class ValidateAuxiliaryData era where
  hashAuxiliaryData :: Core.AuxiliaryData era -> AuxiliaryDataHash (Crypto era)
  validateAuxiliaryData :: Core.AuxiliaryData era -> Bool
