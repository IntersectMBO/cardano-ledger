{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Alonzo.Core (
  ScriptIntegrityHash,
  CoinPerWord (..),
  module Cardano.Ledger.Mary.Core,
)
where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Hashes
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.TreeDiff.Class (ToExpr)
import NoThunks.Class (NoThunks)

type ScriptIntegrityHash c = SafeHash c EraIndependentScriptIntegrity

newtype CoinPerWord = CoinPerWord {unCoinPerWord :: Coin}
  deriving stock (Eq, Ord)
  deriving newtype (EncCBOR, DecCBOR, ToJSON, FromJSON, NFData, NoThunks, ToExpr, Show)
