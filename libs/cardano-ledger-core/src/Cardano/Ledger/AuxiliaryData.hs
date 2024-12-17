{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.AuxiliaryData (
  AuxiliaryDataHash (..),
)
where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Hashes (EraIndependentTxAuxData, SafeHash)
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype AuxiliaryDataHash = AuxiliaryDataHash
  { unsafeAuxiliaryDataHash :: SafeHash EraIndependentTxAuxData
  }
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData, EncCBOR, DecCBOR, ToJSON)
