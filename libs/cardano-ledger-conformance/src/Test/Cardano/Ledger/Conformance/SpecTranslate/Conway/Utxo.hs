{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo () where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (EraPParams (..), EraRule, PParams, Value)
import Cardano.Ledger.Conway.Rules (ConwayUtxoPredFailure)
import Cardano.Ledger.Core (EraTxOut (..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Control.State.Transition.Extended (STS (..))
import Data.Functor.Identity (Identity)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (OpaqueErrorString (..), showOpaqueErrorString)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (SpecTranslate (..))
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..))

instance
  ( SpecRep (PParams era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (UtxoEnv era)
  where
  type SpecRep (UtxoEnv era) = Agda.UTxOEnv

  toSpecRep x =
    Agda.MkUTxOEnv
      <$> toSpecRep (ueSlot x)
      <*> toSpecRep (uePParams x)
      <*> toSpecRep (Coin 10_000_000) -- TODO: Fix generating types

instance
  ( ToExpr (Value era)
  , ToExpr (TxOut era)
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  SpecTranslate ctx (ConwayUtxoPredFailure era)
  where
  type SpecRep (ConwayUtxoPredFailure era) = OpaqueErrorString

  toSpecRep = pure . showOpaqueErrorString
